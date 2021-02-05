package http

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
  def apply(blockingEc: ExecutionContext)(implicit cs: ContextShift[IO]): IO[Server] = {
    def serveStaticFile(request: Request[IO], pathname: String): IO[Response[IO]] = {
      println(s"Looking up $pathname")
      StaticFile.fromFile(new File(pathname), blockingEc, Some(request)).getOrElseF(NotFound())
    }
    val files = Set(
      "client/target/scala-2.12/client-fastopt.js",
      "client/target/scala-2.12/client-fastopt.js.map",
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
    val blockingEc = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
    for {
      server <- Server(blockingEc)
      httpApp = Router("/" -> server.service).orNotFound
      builder = BlazeServerBuilder[IO]
        .bindHttp(7777, "0.0.0.0")
        .withHttpApp(httpApp)
      program <- builder.serve.concurrently(server.effects).compile.drain.map(_ => ExitCode.Success)
    } yield program
//    val nums = Stream.unfold(0)(n => Some(n, n+1))
//    val delays = Stream.fixedDelay(new FiniteDuration(1, MILLISECONDS))
//    val numsDelayed = nums.zip(delays).map { _._1 }
  }
}


//object StreamExt {
//  // https://github.com/functional-streams-for-scala/fs2/blob/v1.0.4/core/shared/src/main/scala/fs2/Chunk.scala#L203
//  implicit class MyChunkExt[+O](chunk: Chunk[O]) {
//    def mapWithStateCarry[S, O2](z: S)(f: (S, O) => (S, O2)): (Chunk[O2], S) =
//      mapWithState_(z)(f)
//    protected def mapWithState_[S, O2](z: S)(f: (S, O) => (S, O2)): (Chunk[O2], S) = {
//      val b = collection.mutable.Buffer.newBuilder[O2]
//      b.sizeHint(chunk.size)
//      var acc = z
//      var i = 0
//      while (i < chunk.size) {
//        val (newAcc, r) = f(acc, chunk.apply(i))
//        acc = newAcc
//        b += r
//        i += 1
//      }
//      Chunk.buffer(b.result) -> acc
//    }
//  }
//
//  implicit class MyStreamExt[+F[_], +O](stream: Stream[F, O]) {
//    // https://github.com/functional-streams-for-scala/fs2/blob/v1.0.4/core/shared/src/main/scala/fs2/Stream.scala#L2172
//    def mapWithState[S, O2](z: S)(f: (S, O) => (S, O2)): Stream[F, O2] =
//      mapWithState_(z)(f).stream
//    private def mapWithState_[S, O2](z: S)(f: (S, O) => (S, O2)): Pull[F, O2, Unit] =
//      stream.pull.uncons.flatMap {
//        case None => Pull.done
//        case Some((hd, tl)) =>
//          val (out, carry) = hd.mapWithStateCarry(z)(f)
//          Pull.output(out) >> MyStreamExt(tl).mapWithState_(carry)(f)
//      }
//
//    // https://github.com/functional-streams-for-scala/fs2/blob/v1.0.4/core/shared/src/main/scala/fs2/Stream.scala#L909
//    def evalMapWithState[F2[x] >: F[x], S, O2](z: S)(f: (S, O) => F2[(S, O2)]): Stream[F2, O2] = {
//      def go(z: S, s: Stream[F2, O]): Pull[F2, O2, Option[Stream[F2, O2]]] =
//        s.pull.uncons1.flatMap {
//          case Some((hd, tl)) =>
//            Pull.eval(f(z, hd)).flatMap {
//              case (z_, o) => Pull.output1(o) >> go(z_, tl)
//            }
//          case None => Pull.pure(None)
//        }
//      stream.pull.uncons1.flatMap {
//        case Some((hd, tl)) =>
//          Pull.eval(f(z, hd)).flatMap { o =>
//            Pull.output(Chunk.seq(List(z, o))) >> go(o._1, tl)
//          }
//        case None => Pull.output1(z) >> Pull.pure(None)
//      }.stream
//    }
//  }
//}


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
//  import StreamExt._

//  private sealed trait MyClientRequest[F[_]]
//  private case class GameStateRequest[F[_]](gameState: Deferred[F, GameState]) extends MyClientRequest[F[_]]
//  private case class ClientGameActionRequest[F[_]](action: Action) extends MyClientRequest[F[_]]
//  private case class NewClientResponse[F[_]](
//    recv: Pipe[F, ClientRequest, Unit],
//    send: Stream[F, Event],
//  )
//  private case class NewClient[F[_]](response: Deferred[F, NewClientResponse[F]])
//  private case class RegisterRequest[F[_]](send: Deferred[F, Stream[F, Event]]) extends MyClientRequest[F[_]]
//  private sealed trait ClientState
//  private case class ConnectedClient(id: ClientId) extends ClientState

  private case class ClientId(id: Long)
  private case class ClientState[F[_]](offer: model.ClientCommand => F[Boolean], close: F[Unit])
  private type Clients[F[_]] = Map[ClientId, ClientState[F]]

  private case class ClientIdGenerator(nextId: Long) {
    def nextClientId: (ClientIdGenerator, ClientId) =
      (ClientIdGenerator(this.nextId + 1), ClientId(this.nextId))
  }

  private sealed trait InstanceEvent[F[_]]
  private case class InstanceClientNew[F[_]](reply: F[Response[F]] => F[Unit]) extends InstanceEvent[F]
  private case class InstanceClientDestroy[F[_]](clientId: ClientId) extends InstanceEvent[F]
  private case class InstanceClientRequest[F[_]](clientId: ClientId, request: model.ClientRequest) extends InstanceEvent[F]

  private case class Instance[F[_]: Concurrent] (
    private val makeRequest: InstanceEvent[F] => F[Unit],
    effects: Stream[F, INothing],
  ) {
    def newClient: F[Response[F]] = {
      for {
        deferred <- Deferred[F, F[Response[F]]]
        result <- makeRequest(InstanceClientNew(deferred.complete))
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
      private val makeRequest: InstanceEvent[F] => F[Unit],
    ) {
      def withEventSend(ev: InstanceEvent[F]): F[InstanceState[F]] = {
        for {
          r <- this.withEvent(ev)
          (instance, events) = r
          _ <- events.evalTap(event =>
            clients.toList.traverse_ {
              case (clientId, client) =>
                for {
                  success <- client.offer(event)
                  _ <- if (success) {
                    ().pure[F]
                  } else {
                    makeRequest(InstanceClientDestroy(clientId))
                  }
                } yield ()
            }
          ).compile.drain
        } yield instance
      }
      private def withEvent: InstanceEvent[F] => F[(InstanceState[F], Stream[F, model.ClientCommand])] = {
        case e: InstanceClientNew[F] => this.withClientNew(e)
        case e: InstanceClientDestroy[F] => this.withClientDestroy(e)
        case e: InstanceClientRequest[F] => this.withClientRequest(e)
      }
      private def withClientNew(ev: InstanceClientNew[F]): F[(InstanceState[F], Stream[F, model.ClientCommand])] = {
        val (newClientIdGen, clientId) = this.clientIdGen.nextClientId
        for {
          r <- Instance.makeClient(
            gameState,
            req => makeRequest(InstanceClientRequest(clientId, req)),
            makeRequest(InstanceClientDestroy(clientId)),
          )
          (client, response) = r
          _ <- ev.reply(response)
        } yield (InstanceState(gameState, clients + (clientId -> client), newClientIdGen, makeRequest), Stream.empty)
      }
      private def withClientDestroy(ev: InstanceClientDestroy[F]): F[(InstanceState[F], Stream[F, model.ClientCommand])] =
        (InstanceState(gameState, clients - ev.clientId, clientIdGen, makeRequest), Stream.empty.covaryAll[F, model.ClientCommand]).pure[F]
      private def withClientRequest(ev: InstanceClientRequest[F]): F[(InstanceState[F], Stream[F, model.ClientCommand])] = {
        ev.request match {
          case model.RequestNewGame =>
            if (gameState.isGameOver) {
              val newGameState = makeNewGame
              val commands: Stream[F, model.ClientCommand] = Stream(model.NewGameState(newGameState))
              (InstanceState(newGameState, clients, clientIdGen, makeRequest), commands).pure[F]
            } else {
              (this, Stream.empty.covaryAll[F, model.ClientCommand]).pure[F]
            }
          case model.ClientGameAction(action) =>
            val events = gameState.generateEvents(action)
            val newGameState = events.foldLeft(gameState) { case (gs, e) => gs.withEvent(e) }
            val commands: Stream[F, model.ClientCommand] = Stream.emits(events.map(model.GameEvent))
            (InstanceState(newGameState, clients, clientIdGen, makeRequest), commands).pure[F]
        }
      }
    }

    private def makeClient[F[_]: Concurrent](
      initialGameState: model.GameState,
      makeRequest: model.ClientRequest => F[Unit],
      connectionClosed: F[Unit],
    ): F[(ClientState[F], F[Response[F]])] = {
      val encodeClientCommand: model.ClientCommand => WebSocketFrame =
        command => WebSocketFrame.Text(command.asJson.noSpaces)
      val encodeClientCommands: Pipe[F, model.ClientCommand, WebSocketFrame] =
        commands => commands.map(encodeClientCommand)

      sealed trait RecvError
      case object ConnectionClosed extends RecvError
      case class DecodeError(message: String, err: Error) extends RecvError
      case class UnknownFrameType(frame: WebSocketFrame) extends RecvError
      val decodeClientRequests: Pipe[F, WebSocketFrame, Either[RecvError, model.ClientRequest]] =
        frames => frames.map {
          case WebSocketFrame.Text((rawMessage, _)) =>
            decode[model.ClientRequest](rawMessage).leftMap {
              err => DecodeError(rawMessage, err)
            }
          case WebSocketFrame.Close(_) => Left(ConnectionClosed)
          case other => Left(UnknownFrameType(other))
        }

      val handleClientRequest: model.ClientRequest => F[Unit] = makeRequest
      val handleClientError: RecvError => F[Unit] = {
        case ConnectionClosed =>
          connectionClosed
        case other =>
          println(other)
          connectionClosed
      } // TODO: better way which doesn't repeat `connectionClosed`?
      val handleDecodedFrames: Pipe[F, Either[RecvError, model.ClientRequest], Unit] =
        decoded => decoded.evalMap {
          case Right(request) => handleClientRequest(request)
          case Left(err) => handleClientError(err)
        }

      for {
        sendQueue <- Queue.boundedNoneTerminated[F, model.ClientCommand](100)
      } yield (
        ClientState(
          offer = command => sendQueue.offer1(Some(command)),
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
        instanceEvents <- Queue.unbounded[F, InstanceEvent[F]]
        initialInstanceState = InstanceState[F](
          initialGameState,
          Map(),
          ClientIdGenerator(nextId = 1),
          instanceEvents.enqueue1,
        )
        initialInstance = Instance[F](
          instanceEvents.enqueue1,
          instanceEvents.dequeue
            .evalScan(initialInstanceState) {
              case (instance, ev) => instance.withEventSend(ev)
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
      override def getWS(req: Request[F]): F[Response[F]] = {
        instance.newClient
      }
    }
  }
}
//class MultiplayerGame()(implicit val cs: ContextShift[IO]) {
//  // TODO have something that ticks the game every "frame"
//  def makeNewGame: GameState = GameStateFactory.newGame(24, 30, 100)
//
//  var gameState: GameState = makeNewGame
//  case class ClientData(commandQueue: Queue[IO, ClientCommand])
//  val actionQueue: Queue[IO, Action] = Queue.unbounded[IO, Action].unsafeRunSync
//  val connectedClients: mutable.Set[ClientData] = mutable.Set[ClientData]()
//
////  val actions: IO[Queue[IO, Action]] = Queue.unbounded[IO, Action]
//  sealed trait ClientAction
//  case class ClientEnter() extends ClientAction
//  case class ClientLeave() extends ClientAction
//
//  // ClientRequest -> ... -> broadcast ClientCommand
////  val clientCommands = Topic[IO, ClientCommand]
////  val clients: Pipe[IO, ClientAction, Unit] = ???
////  val clientActions: Pipe[IO, WebSocketFrame, ClientAction] = {
////    case WebSocketFrame.Close(_) =>
////    case WebSocket
////  }
//  val ctions: IO[Topic[IO, ClientAction]] = Topic[IO, ClientAction](null)
//  val gameStates: Stream[IO, GameState] = Stream(makeNewGame) ++ gameStates.zip(actions).map {
//    case (gameState, action) =>
//      val events = gameState.generateEvents(action)
//      events.foldLeft(gameState) { case (gs, e) => gs.withEvent(e) }
//  }
////  def sendToAllClientsS(command: ClientCommand)
//  def sendToAllClients(command: ClientCommand): IO[Unit] = {
//    val tasks = connectedClients.map { c =>
//      //println(s"sending ${msg} to some client who know??")
//      c.commandQueue.enqueue1(command)
//    }
//    // TODO make the above run in parallel
//     tasks.toList.sequence_
//  }
//
//  actionQueue.dequeue.flatMap { action =>
//    gameState.synchronized {
//      val events = gameState.generateEvents(action)
//      gameState = events.foldLeft(gameState) { case (gs, e) => gs.withEvent(e) }
//      Stream.emits(events)
//    }
//  }.evalMap(e => sendToAllClients(GameEvent(e))).unsafeRunAsync (e => ())
//  //}.evalMap(e => sendToAllClients(GameEvent(e))).run.attempt.collect {
//  //  case Left(e: Throwable) => e.printStackTrace()
//  //} unsafeRunAsync (e => ())
//
//  def getWS(req: Request[IO]): IO[Response[IO]] = {
//    val rawMessageQueue = Queue.unbounded[IO, WebSocketFrame].unsafeRunSync
//
//    val client = ClientData(
//      commandQueue = Queue.unbounded[IO, ClientCommand].unsafeRunSync
//    )
//
//    connectedClients.add(client)
//    println("Client connected")
//
//    val procTask = rawMessageQueue.dequeue.map {
//      case WebSocketFrame.Close(_) =>
//        println("Client disconnected")
//        connectedClients.remove(client)
//        None
//      case WebSocketFrame.Text(message, _) =>
//        println(s"Client message $message")
//        (for {
//          clientMessage <- decode[ClientRequest](message)
//        } yield clientMessage) match {
//          case Left(e) => println(e); None
//          case Right(RequestNewGame) =>
//            println("Client decoded message RequestNewGame")
//            // TODO move this logic out to a game controller or something
//            if (gameState.isGameOver) {
//              gameState = makeNewGame
//              // TODO make this part of the stream or something
//              sendToAllClients(NewGameState(gameState)).unsafeRunSync()
//            } else {
//              println("Client trying to request new game before game is over")
//            }
//            None
//          case Right(clientRequest) =>
//            println(s"Client decoded message $clientRequest")
//            Some(clientRequest)
//        }
//    } collect {
//      case Some(ClientGameAction(action)) => action
//    } to actionQueue.enqueue
//
//    procTask.run unsafeRunAsync (e => ())
//
//    WebSocketBuilder[IO].build(
//      (Stream eval IO(
//        WebSocketFrame.Text(NewGameState(gameState).asJson.noSpaces)
//      )) ++ client.commandQueue.dequeue.map(c => WebSocketFrame.Text(c.asJson.noSpaces)),
//      rawMessageQueue.enqueue
//    )
//  }
//}
