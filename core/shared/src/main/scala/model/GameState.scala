package model

import scala.concurrent.duration._
import scala.util.Random

sealed trait Entity

sealed trait CellState

case object Empty extends CellState
case object Bomb extends CellState

sealed trait CellModifier

case object Revealed extends CellModifier
case object Flagged extends CellModifier
case object Hidden extends CellModifier

case class Cell(state: CellState, modifier: CellModifier)

sealed trait VictoryState

case object Win extends VictoryState
case object Lose extends VictoryState
case object Pending extends VictoryState

//case class ClientId(id: Int)
//case class Client(id: ClientId, name: String, color: String)
//
//case class CharacterId(id: Int)
//case class Character(
//  id: CharacterId, owner: ClientId,
//  row: Int, col: Int, heading: Direction, lives: Int
//)

case class GameState(
  // cells[row][col]. 0 0 is top left
  board: Array[Array[Cell]],
  tick: Long,
  victoryState: VictoryState
) {
  def isGameOver: Boolean = victoryState != Pending
  def numRows: Int = board.length
  def numCols: Int = if (numRows == 0) 0 else board(0).length
  private def isValidCell(row: Int, col: Int): Boolean = {
    if      (row < 0 || row >= numRows) false
    else if (col < 0 || col >= numCols) false
    else true
  }
  private def isAtValidCell(action: CellAction): Boolean = {
    isValidCell(action.row, action.col)
  }
  private def isBombClick(action: CellAction): Boolean = {
    action match {
      case Reveal(row, col) => board(row)(col).state == Bomb
      case _ => false
    }
  }
  private def calcValidAdjacentCells(row: Int, col: Int): Seq[(Int, Int)] = for {
    r <- row - 1 to row + 1
    c <- col - 1 to col + 1
    if isValidCell(r, c)
  } yield (r, c)

  private def numAdjacentBombs(board: Array[Array[Cell]], row: Int, col: Int): Int = {
    calcValidAdjacentCells(row, col).count { case (r, c) => board(r)(c).state == Bomb }
  }
  private def reveal(board: Array[Array[Cell]], row: Int, col: Int): Array[Array[Cell]] = {
    board(row).update(col, board(row)(col).copy(modifier = Revealed ))
    if (numAdjacentBombs(board, row, col) > 0) {
      board
    } else {
      calcValidAdjacentCells(row, col)
        .filterNot { case (r, c) => board(r)(c).modifier == Revealed }
        .foldLeft(board) { case (b, (r, c)) => reveal(b, r, c) }
    }
  }

  private def gameWin(board: Array[Array[Cell]]): Boolean = {
    !board.flatten.exists(c => c.state == Empty && c.modifier != Revealed)
  }

  def generateEvents(action: Action): Seq[Event] = action match {
    case action: CellAction if isAtValidCell(action) && !isGameOver =>
      Seq(Event(action))
    case _ =>
      Seq.empty
  }

  def withEvent(event: Event): GameState = event.action match {
    case action: CellAction if isAtValidCell(action) && !isGameOver =>
      GameState(
        board = action match {
          case Reveal(row, col) => if (isBombClick(action)) {
            val newBoard = board.clone()
            newBoard(row).update(col, board(row)(col).copy(modifier = Revealed))
            newBoard
          } else {
            reveal(board.clone(), action.row, action.col)
          }
          case Flag(row, col) => if (board(row)(col).modifier == Hidden) {
            val newBoard = board.clone()
            newBoard(row).update(col, board(row)(col).copy(modifier = Flagged))
            newBoard
          } else {
            board
          }
          case Unflag(row, col) => if (board(row)(col).modifier == Flagged) {
            val newBoard = board.clone()
            newBoard(row).update(col, board(row)(col).copy(modifier = Hidden))
            newBoard
          } else {
            board
          }
        },
        tick = tick,
        victoryState = if (isBombClick(action)) Lose else if (gameWin(board)) Win else victoryState
      )
    case _ => this
  }

  def asAsciiString: String = (for {
    (cells, r) <- board.zipWithIndex
  } yield {
    (for {
      (cell, c) <- cells.zipWithIndex
    } yield cell match {
      case Cell(_, Hidden) => "#"
      case Cell(_, Flagged) => "F"
      case Cell(Bomb, Revealed) => "*"
      case Cell(_, Revealed) =>
        val count = numAdjacentBombs(board, r, c)
        if (count == 0) " "
        else count.toString
    }).mkString
  }) mkString "\n"
}



// sealed trait CharacterAction extends Action
// 
//   sealed trait Direction
//     case object North extends Direction
//     case object South extends Direction
//     case object East extends Direction
//     case object West extends Direction
// 
//   case class AddCharacter(name: String, color: String, row: Int, col: Int) extends CharacterAction
//   case class FaceDirection(id: CharacterId, direction: Direction) extends CharacterAction
//   case class WalkForward(id: CharacterId) extends CharacterAction
//   case class FlagForward(id: CharacterId) extends CharacterAction
// 
// 
// 
// case class ClientAction(id: ClientId, action: Action)



sealed trait Action

  sealed trait CellAction extends Action {
    val row: Int
    val col: Int
  }
  case class Reveal(row: Int, col: Int) extends CellAction
  case class Flag(row: Int, col: Int) extends CellAction
  case class Unflag(row: Int, col: Int) extends CellAction

case class Event(action: Action)
