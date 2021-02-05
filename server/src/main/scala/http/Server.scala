package http

import cats.Alternative

import java.io.File
import java.util.concurrent.Executors
import scala.language.higherKinds
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}
import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global

import cats.implicits._
import cats.effect._
import cats.Applicative

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.concurrent.Deferred

import fs2.Stream
import fs2.{Chunk, INothing, Pipe, Pull, Pure}
import fs2.concurrent.{NoneTerminatedQueue, Queue, Topic}

import org.http4s.{HttpApp, HttpRoutes, HttpService, Request, Response, StaticFile}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.{BlazeBuilder, BlazeServerBuilder}
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.server.Router

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._



abstract class Server {
  def effects: Stream[IO, INothing]
  def service: HttpRoutes[IO]
}

object Server {
  def apply(blockingEc: Blocker)(implicit cs: ContextShift[IO]): IO[Server] = {
    def serveStaticFile(request: Request[IO], pathname: String): IO[Response[IO]] = {
      println(s"Looking up $pathname")
      StaticFile.fromFile(new File(pathname), blockingEc, Some(request)).getOrElseF(NotFound())
    }
    val files = Set(
      "client/target/scala-2.13/client-fastopt.js",
      "client/target/scala-2.13/client-fastopt.js.map",
    )
    for {
      game <- MultiplayerGame[IO]
    } yield new Server {
      override def effects: Stream[IO, INothing] = game.effects
      override def service: HttpRoutes[IO] = HttpRoutes.of[IO] {
        case r @ GET -> Root / "multiplayer" => game.getWS(r)
        case r @ GET -> Root / "ping" => Ok("pong")
        case r @ GET -> (Root | Root / "index.html") =>
//          println(s"${StaticFile.fromFile(new File("./index-dev.html"), blockingEc, Some(r)).getOrElseF(NotFound()).unsafeRunSync.headers}")
          serveStaticFile(r, "./index-dev.html")
        case r @ GET -> url if files.contains(url.toString.substring(1)) =>
          serveStaticFile(r, s"./$url")
        case r @ GET -> "/" /: url => println(s"Not found a ${url.toString}"); NotFound()
        case r @ GET -> url => println(s"Not found ${url.toString}"); NotFound()
        case r => println(s"Not found $r"); NotFound()
      }
    }
  }
}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    for {
      server <- Server(Blocker.liftExecutionContext(ec))
      httpApp = Router("/" -> server.service).orNotFound
      builder = BlazeServerBuilder[IO](ec)
        .bindHttp(7777, "0.0.0.0")
        .withHttpApp(httpApp)
      program <- builder.serve.concurrently(server.effects).compile.drain.map(_ => ExitCode.Success)
    } yield program
  }
}


/**
  * wow, such multiplayer game
  *
  * protocol:
  *
  * on tick:
  *  tick game with queue of actions (clearing queue)
  *  send new gamestate to all clients
  *
  * on message recv:
  *  add to queue of actions
  *
  */
abstract class MultiplayerGame[F[_]] { self =>
  def effects: Stream[F, INothing]
  def getWS(req: Request[F]): F[Response[F]]
}
object MultiplayerGame {
  private case class ClientId(id: Long)
  private case class ClientState[F[_]](enqueueMsg: model.ServerMsg => F[Boolean], close: F[Unit])
  private type Clients[F[_]] = Map[ClientId, ClientState[F]]

  private case class ClientIdGenerator(nextId: Long) {
    def nextClientId: (ClientIdGenerator, ClientId) =
      (ClientIdGenerator(this.nextId + 1), ClientId(this.nextId))
  }

  private sealed trait InstanceCmd[F[_]]
  private case class InstanceClientNew[F[_]](reply: F[Response[F]] => F[Unit]) extends InstanceCmd[F]
  private case class InstanceClientDestroy[F[_]](clientId: ClientId) extends InstanceCmd[F]
  private case class InstanceClientRequest[F[_]](clientId: ClientId, request: model.ClientMsg) extends InstanceCmd[F]

  private case class Instance[F[_]: Concurrent] (
    private val enqueueCmd: InstanceCmd[F] => F[Unit],
    effects: Stream[F, INothing],
  ) {
    def cmdClientNew: F[Response[F]] = {
      for {
        deferred <- Deferred[F, F[Response[F]]]
        result <- enqueueCmd(InstanceClientNew(deferred.complete))
        () = result
        response <- deferred.get
        r <- response
      } yield r
    }
  }
  private object Instance {
    private case class InstanceState[F[_]: Concurrent] (
      private val gameState: model.GameState,
      private val clients: Clients[F],
      private val clientIdGen: ClientIdGenerator,
      private val enqueueCmd: InstanceCmd[F] => F[Unit],
    ) {
      def handleCmd(cmd: InstanceCmd[F]): F[InstanceState[F]] = {
        for {
          r <- this.handleCmdWithMsgs(cmd)
          (newInstance, msgs) = r
          _ <- msgs.evalTap(msg =>
            clients.toList.traverse_ {
              case (clientId, client) =>
                for {
                  success <- client.enqueueMsg(msg)
                  _ <- if (success) ().pure[F] else enqueueCmd(InstanceClientDestroy(clientId))
                } yield ()
            }
          ).compile.drain
        } yield newInstance
      }
      private def handleCmdWithMsgs: InstanceCmd[F] => F[(InstanceState[F], Stream[F, model.ServerMsg])] = {
        case e: InstanceClientNew[F] => this.handleClientNew(e)
        case e: InstanceClientDestroy[F] => this.handleClientDestroy(e)
        case e: InstanceClientRequest[F] => this.handleClientRequest(e)
      }
      private def handleClientNew(ev: InstanceClientNew[F]): F[(InstanceState[F], Stream[F, model.ServerMsg])] = {
        val (newClientIdGen, clientId) = this.clientIdGen.nextClientId
        for {
          r <- Instance.makeClient(
            gameState,
            msg => enqueueCmd(InstanceClientRequest(clientId, msg)),
            enqueueCmd(InstanceClientDestroy(clientId)),
          )
          (client, response) = r
          _ <- ev.reply(response)
        } yield (InstanceState(gameState, clients + (clientId -> client), newClientIdGen, enqueueCmd), Stream.empty)
      }
      private def handleClientDestroy(ev: InstanceClientDestroy[F]): F[(InstanceState[F], Stream[F, model.ServerMsg])] =
        (InstanceState(gameState, clients - ev.clientId, clientIdGen, enqueueCmd), Stream.empty.covaryAll[F, model.ServerMsg]).pure[F]
      private def handleClientRequest(ev: InstanceClientRequest[F]): F[(InstanceState[F], Stream[F, model.ServerMsg])] = {
        ev.request match {
          case model.RequestNewGame =>
            if (gameState.isGameOver) {
              val newGameState = makeNewGame
              val msgs: Stream[F, model.ServerMsg] = Stream(model.NewGameState(newGameState))
              (InstanceState(newGameState, clients, clientIdGen, enqueueCmd), msgs).pure[F]
            } else {
              (this, Stream.empty.covaryAll[F, model.ServerMsg]).pure[F]
            }
          case model.ClientGameAction(action) =>
            val events = gameState.generateEvents(action)
            val newGameState = events.foldLeft(gameState) { case (gs, e) => gs.withEvent(e) }
            val msgs: Stream[F, model.ServerMsg] = Stream.emits(events.map(model.GameEvent))
            (InstanceState(newGameState, clients, clientIdGen, enqueueCmd), msgs).pure[F]
        }
      }
    }

    private def makeClient[F[_]: Concurrent](
      initialGameState: model.GameState,
      handleClientMsg: model.ClientMsg => F[Unit],
      handleConnectionClosed: F[Unit],
    ): F[(ClientState[F], F[Response[F]])] = {
      val encodeClientCommand: model.ServerMsg => WebSocketFrame =
        command => WebSocketFrame.Text(command.asJson.noSpaces)
      val encodeClientCommands: Pipe[F, model.ServerMsg, WebSocketFrame] =
        commands => commands.map(encodeClientCommand)

      sealed trait RecvError
      case object ConnectionClosed extends RecvError
      case class DecodeError(message: String, err: Error) extends RecvError
      case class UnknownFrameType(frame: WebSocketFrame) extends RecvError
      val decodeClientRequests: Pipe[F, WebSocketFrame, Either[RecvError, model.ClientMsg]] =
        frames => frames.map {
          case WebSocketFrame.Text((rawMessage, _)) =>
            decode[model.ClientMsg](rawMessage).leftMap {
              err => DecodeError(rawMessage, err)
            }
          case WebSocketFrame.Close(_) => Left(ConnectionClosed)
          case other => Left(UnknownFrameType(other))
        }

      val handleClientRequest: model.ClientMsg => F[Unit] = handleClientMsg
      def handleClientError(err: RecvError): F[Unit] = for {
        _ <- if (err == ConnectionClosed) ().pure[F] else Sync[F].delay(println(err))
        _ <- handleConnectionClosed
      } yield ()
      val handleDecodedFrames: Pipe[F, Either[RecvError, model.ClientMsg], Unit] =
        decoded => decoded.evalMap {
          case Right(request) => handleClientRequest(request)
          case Left(err) => handleClientError(err)
        }

      for {
        sendQueue <- Queue.boundedNoneTerminated[F, model.ServerMsg](100)
      } yield (
        ClientState(
          enqueueMsg = command => sendQueue.offer1(Some(command)),
          close = sendQueue.enqueue1(None),
        ),
        WebSocketBuilder[F].build(
          Stream(encodeClientCommand(model.NewGameState(initialGameState))) ++
            sendQueue.dequeue.through(encodeClientCommands),
          decodeClientRequests.andThen(handleDecodedFrames),
        ),
      )
    }
    def build[F[_]: Concurrent](
      initialGameState: model.GameState,
    ): F[Instance[F]] = {
      for {
        cmds <- Queue.unbounded[F, InstanceCmd[F]]
        initialInstanceState = InstanceState[F](
          initialGameState,
          Map(),
          ClientIdGenerator(nextId = 1),
          cmds.enqueue1,
        )
        initialInstance = Instance[F](
          cmds.enqueue1,
          cmds.dequeue
            .evalScan(initialInstanceState) {
              case (instanceState, ev) => instanceState.handleCmd(ev)
            }
            .drain,
        )
      } yield initialInstance
    }
  }

  private def makeNewGame: model.GameState = model.GameStateFactory.newGame(24, 30, 100)

  def apply[F[_]: Concurrent]: F[MultiplayerGame[F]] = {
//    val clientIds = Stream.unfold(0)(n => Some((ClientId(n), n+1)))
    for {
      instance <- Instance.build(makeNewGame)
    } yield new MultiplayerGame[F] {
      override def effects: Stream[F, INothing] = instance.effects
      override def getWS(req: Request[F]): F[Response[F]] = instance.cmdClientNew
    }
  }
}
