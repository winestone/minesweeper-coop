package model

import scala.util.Random

object GameStateFactory {
  def newGame(rows: Int, cols: Int, numMines: Int): GameState = {
    val bombLocations = Random.shuffle(for {
      r <- 0 until rows
      c <- 0 until cols
    } yield (r, c)).take(numMines).toSet
    GameState(
      board = for {
        row <- (0 until rows).toArray
      } yield for {
        col <- (0 until cols).toArray
      } yield {
        Cell(
          state = if (bombLocations.contains((row, col))) Bomb else Empty,
          modifier = Hidden
        )
      },
      tick = 0,
      victoryState = Pending
    )
  }
}
