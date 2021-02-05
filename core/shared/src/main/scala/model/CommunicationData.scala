package model

sealed trait ClientRequest
  case object RequestNewGame extends ClientRequest
  case class ClientGameAction(action: Action) extends ClientRequest

sealed trait ClientCommand
  case class NewGameState(gameState: GameState) extends ClientCommand
  case class GameEvent(event: Event) extends ClientCommand
