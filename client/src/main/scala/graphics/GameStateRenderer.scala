package graphics

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, MouseEvent}
import org.scalajs.dom.raw.WebSocket

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.annotation._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._


// TODO: add counter for (total bombs - flags)
// TODO: add counter for (total cells - bombs - hidden cells)

// TODO:
//   names, colors
//   click to place character
//   WASD to change direction, and move
//   walking over cell will reveal, if unflagged bomb, die
//   space to flag
//   lose condition: no alive players

class GameStateRenderer(canvas: dom.html.Canvas) {
  val ctx: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  def renderBoard(gameState: model.GameState, x: Int, y: Int, w: Int, h: Int): Unit = {
    val CELL_BORDER_SIZE = 2

    ctx.fillStyle = "rgb(128, 128, 128)"
    ctx.fillRect(x, y, w, h)

    val xOffset = x + CELL_BORDER_SIZE
    val yOffset = y + CELL_BORDER_SIZE
    val cellWidth = (w.toDouble - 2 * CELL_BORDER_SIZE) / gameState.numCols
    val cellHeight = (h.toDouble - 2 * CELL_BORDER_SIZE) / gameState.numRows
    val fontHeight = (math.min(cellWidth, cellHeight) - 4 * CELL_BORDER_SIZE).floor * 0.8
    ctx.font = "700 " + fontHeight + "px sans-serif"
    for {
      row <- 0 until gameState.numRows
      col <- 0 until gameState.numCols
    } yield {
      val startX = xOffset + (col * cellWidth).floor + CELL_BORDER_SIZE
      val startY = yOffset + (row * cellHeight).floor + CELL_BORDER_SIZE
      val endX = xOffset + ((col + 1) * cellWidth).floor - CELL_BORDER_SIZE
      val endY = yOffset + ((row + 1) * cellHeight).floor - CELL_BORDER_SIZE
      def renderFullRect(color: String): Unit = {
        ctx.fillStyle = color
        ctx.fillRect(startX, startY, endX - startX, endY - startY)
      }
      def renderTextMiddle(text: String, color: Option[String] = None): Unit = {
        color match {
          case Some(x) =>
            ctx.fillStyle = x
          case None => ()
        }
        val textMetric = ctx.measureText(text)
        ctx.fillText(text, startX + ((endX - startX - textMetric.width) / 2), startY + CELL_BORDER_SIZE + fontHeight)
      }
      gameState.board(row)(col) match {
        case model.Cell(_, model.Hidden) =>
          renderFullRect("rgb(192, 192, 192)")
        case model.Cell(_, model.Flagged) =>
          renderFullRect("rgb(255, 255, 0)")
          renderTextMiddle("\u2691", Some("rgb(0, 0, 0)"))
        case model.Cell(model.Bomb, model.Revealed) =>
          renderFullRect("rgb(255, 0, 0)")
          renderTextMiddle("\uD83D\uDCA3", Some("rgb(0, 0, 0)"))
        case model.Cell(_, model.Revealed) =>
          val count = gameState.numAdjacentBombs(row, col)
          renderFullRect("rgb(124, 252, 0)")
          if (count != 0) {
            renderTextMiddle(count.toString, Some("rgb(0, 0, 0)"))
          }
      }
    }
  }

  def render(gameState: model.GameState): Unit = {
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    canvas.width = gameState.numCols * 30
    canvas.height = gameState.numRows * 30

    renderBoard(gameState, 0, 0, gameState.numCols * 30, gameState.numRows * 30)
  }
}

class GameStateClient(gameDiv: dom.html.Div, webSocketUrl: String) {
  private val canvas = gameDiv.querySelector(".canvas").asInstanceOf[dom.html.Canvas]
  private val retryPopup = gameDiv.querySelector(".retryPopup").asInstanceOf[dom.html.Div]
  private val retryButton = retryPopup.querySelector(".retryButton").asInstanceOf[dom.html.Button]
  private val winText = retryPopup.querySelector(".winText").asInstanceOf[dom.html.Div]
  private val loseText = retryPopup.querySelector(".loseText").asInstanceOf[dom.html.Div]
  private var webSocket: dom.WebSocket = null

  private var gameState = model.GameStateFactory.newGame(10, 10, 10)
  private val gameStateRenderer = new GameStateRenderer(canvas)

  sealed trait Service {
    var started_ = false
    def started: Boolean = started_
    def startup(): Unit =
      started_ = true
    def shutdown(): Unit =
      started_ = false
  }

  def sendRequest(request: model.ClientMsg): Unit = {
    webSocket.send(request.asJson.noSpaces)
  }

  private object WebSocketInterface extends Service {
    override def startup(): Unit = {
      super.startup()
      webSocket = new dom.WebSocket(webSocketUrl)
      webSocket.onopen = handleWebSocketOpen _
      webSocket.onmessage = handleWebSocketMessage _
      webSocket.onclose = handleWebSocketClose _
    }
    private def handleWebSocketOpen(e: dom.Event): Unit = {
      sendRequest(model.RequestNewGame)
    }
    private def handleWebSocketMessage(e: dom.MessageEvent): Unit = {
      decode[model.ServerMsg](e.data.toString) match {
        case Left(err) => println(err); None
        case Right(model.NewGameState(gs)) =>
          gameState = gs
        case Right(model.GameEvent(event)) =>
          gameState = gameState.withEvent(event)
      }
      gameStateRenderer.render(gameState)
      if (gameState.isGameOver) {
        if (MainGameInterface.started) {
          MainGameInterface.shutdown()
          RetryPopupInterface.startup()
        }
      } else {
        if (RetryPopupInterface.started) {
          MainGameInterface.startup()
          RetryPopupInterface.shutdown()
        }
      }
    }
    private def handleWebSocketClose(e: dom.CloseEvent): Unit = {
      shutdown()
      setTimeout(1000) { startup _ }
    }
    override def shutdown(): Unit = {
      super.shutdown()
      if ((webSocket.readyState == dom.WebSocket.CONNECTING) || (webSocket.readyState == dom.WebSocket.OPEN)) {
        webSocket.onmessage = null
        webSocket.onclose = null
        webSocket.close()
      }
    }
  }

  private sealed trait MouseAction
  private case object LeftAction extends MouseAction
  private case object RightAction extends MouseAction
  private case object LeftRightAction extends MouseAction

  private object MainGameInterface extends Service {
    override def startup(): Unit = {
      super.startup()
      canvas.addEventListener("contextmenu", handleMouseEventFunc)
      canvas.addEventListener("click", handleMouseEventFunc)
    }
    private def handleMouseClick(mouseAction: MouseAction, x: Int, y: Int): Unit = {
      if (WebSocketInterface.started) {
        val CELL_BORDER_SIZE = 2
        val w = gameState.numCols * 30
        val h = gameState.numRows * 30
        val cellWidth = (w.toDouble - 2 * CELL_BORDER_SIZE) / gameState.numCols
        val cellHeight = (h.toDouble - 2 * CELL_BORDER_SIZE) / gameState.numRows

        val row = (y / cellHeight).floor.toInt
        val col = (x / cellWidth).floor.toInt
        val actions: Seq[model.Action] = (mouseAction match {
          case LeftAction => Seq(model.Reveal(row, col))
          case RightAction => Seq((gameState.board(row)(col).modifier match {
            case model.Flagged => model.Unflag
            case model.Hidden => model.Flag
            case _ => model.Reveal
          })(row, col))
          case LeftRightAction => {
            val numAdjacentFlagged = gameState.validAdjacentCells(row, col)
              .count { case (_, _, cell) => cell.modifier == model.Flagged }
            if (gameState.numAdjacentBombs(row, col) == numAdjacentFlagged) {
              gameState.validAdjacentCells(row, col)
                .filter { case (_, _, cell) => cell.modifier == model.Hidden }
                .map { case (r, c, _) => model.Reveal(r, c) }
            } else {
              Seq()
            }
          }
        })
        actions.foreach(action => sendRequest(model.ClientGameAction(action)))

        gameStateRenderer.render(gameState)
      }
    }
    private def handleMouseEvent(e: dom.MouseEvent): Unit = {
      // `e.buttons` doesn't include the button triggering the `click` event
      // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button
      // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons
      val buttons = e.buttons + Seq(1, 4, 2, 8, 16)(e.button)
      (buttons match {
        case 1 => Some(LeftAction)
        case 2 => Some(RightAction)
        case 3 => Some(LeftRightAction)
        case _ => None
      }).foreach(mouseAction => {
        handleMouseClick(mouseAction, (e.pageX - canvas.offsetLeft).toInt, (e.pageY - canvas.offsetTop).toInt) // http://stackoverflow.com/a/14872192/3492895
        e.preventDefault()
        e.stopPropagation()
      })
    }
    val handleMouseEventFunc: js.Function1[dom.MouseEvent, Unit] = handleMouseEvent _
    override def shutdown(): Unit = {
      super.shutdown()
      canvas.removeEventListener("contextmenu", handleMouseEventFunc)
      canvas.removeEventListener("click", handleMouseEventFunc)
    }
  }

  private object RetryPopupInterface extends Service {
    override def startup(): Unit = {
      super.startup()
      retryPopup.style.display = ""
      gameState.victoryState match {
        case model.Win =>
          winText.style.display = ""
        case model.Lose =>
          loseText.style.display = ""
        case model.Pending => throw new Exception("gameState.victoryState was not expected to be of value model.Pending")
      }
      retryButton.addEventListener("click", handleRetryButtonClickFunc)
    }
    private def handleRetryButtonClick(e: dom.MouseEvent): Unit = {
      if (WebSocketInterface.started) {
        sendRequest(model.RequestNewGame)
      }
    }
    val handleRetryButtonClickFunc: js.Function1[dom.MouseEvent, Unit] = handleRetryButtonClick _
    override def shutdown(): Unit = {
      super.shutdown()
      retryPopup.style.display = "none"
      winText.style.display = "none"
      loseText.style.display = "none"
      retryButton.removeEventListener("click", handleRetryButtonClickFunc)
    }
  }

  WebSocketInterface.startup()
  MainGameInterface.startup()

  def shutdown(): Unit = {
    WebSocketInterface.shutdown()
    MainGameInterface.shutdown()
    RetryPopupInterface.shutdown()
  }
}

@JSExportTopLevel("Main")
object Main {
  @JSExport
  def main(args: Array[String]): Unit = {
    {
      import scalatags.JsDom.all._

      dom.document.body.appendChild(div(
        div(id:="menu")(
          input(`class`:="connectInput", value:={
            val loc = dom.window.location
            val protocol = loc.protocol match {
              case "https:" => "wss"
              case _ => "ws"
            }
            val hostname = if (loc.hostname.isEmpty) "localhost" else loc.hostname
            val port = if (loc.port.isEmpty) "" else s":${loc.port}"
            s"$protocol://$hostname$port/multiplayer"
          }),
          button(`class`:="connectButton")("Connect")
        ),
        div(css("display"):="none", id:="game")(
          canvas(`class`:="canvas"),
          div(css("display"):="none", `class`:="retryPopup")(
            div(css("display"):="none", `class`:="winText")("You've won!"),
            div(css("display"):="none", `class`:="loseText")("You've lost!"),
            button(`class`:="retryButton")("Play again")
          )
        )
      ).render)
    }

    val menu = dom.document.querySelector("#menu").asInstanceOf[dom.html.Div]
    val menu_connectInput = dom.document.querySelector("#menu .connectInput").asInstanceOf[dom.html.Input]
    val menu_connectButton = dom.document.querySelector("#menu .connectButton").asInstanceOf[dom.html.Button]
    val game = dom.document.querySelector("#game").asInstanceOf[dom.html.Div]

    def handleConnectButtonClick(e: dom.MouseEvent): Unit = {
      val webSocketUrl = menu_connectInput.value
      menu_connectButton.removeEventListener("click", handleConnectButtonClick _)
      menu.style.display = "none"
      game.style.display = ""
      val _ = new GameStateClient(game, webSocketUrl)
    }
    menu_connectButton.addEventListener("click", handleConnectButtonClick _)
  }
}
