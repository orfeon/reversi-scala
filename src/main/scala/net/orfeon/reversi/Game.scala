package net.orfeon.reversi

object Game extends App {

  val board = Board()
  val player1 = new CustomPlayer(Board.StoneBlack)
  val player2 = new CustomPlayer(Board.StoneWhite)
  while(!board.checkGameOver) {
    val position1 = player1.think(board)
    println(position1)
    board.set(position1)
    board.show
    val position2 = player2.think(board)
    println(position2)
    board.set(position2)
    board.show
  }
}

class CustomPlayer(val stone: Int) extends Player(stone) {
  // customize your board evaluation function
  override def evaluate(board: Board, stone: Int): Int = {
    if(board.count(Board.StoneBlank) < 10) {
      return board.count(stone) - board.count(-stone)
    }
    4 * board.count(stone) - board.count(-stone) +
      10 * (board.countCornerStone(stone) - board.countCornerStone(-stone)) +
       5 * (board.countMobility(stone) - board.countMobility(-stone)) +
       3 * (board.countLiverty(stone) - board.countLiverty(-stone)) +
      20 * (board.countStableStone(stone) - board.countStableStone(-stone))
  }
  // customize your think depth function
  override def depth(board: Board): Int = {
    if(board.count(Board.StoneBlank) < 10) {
      10
    } else {
      5
    }
  }
}