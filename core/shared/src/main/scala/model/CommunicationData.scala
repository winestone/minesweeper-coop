package model

sealed trait ClientMsg
  case object RequestNewGame extends ClientMsg
  case class ClientGameAction(action: Action) extends ClientMsg

sealed trait ServerMsg
  case class NewGameState(gameState: GameState) extends ServerMsg
  case class GameEvent(event: Event) extends ServerMsg
