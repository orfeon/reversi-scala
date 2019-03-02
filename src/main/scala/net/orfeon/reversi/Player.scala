package net.orfeon.reversi

class Player(stone: Int) {

  def think(board: Board, depth: Int): Position = {
    val movables = board.calcMovable(this.stone)
    movables.length match {
      case 0 => Position(this.stone)
      case 1 => Position(movables(0)._1, movables(0)._2, this.stone, 0)
      case _ =>
        val (x, y, score) = alphabeta(board, this.stone, -10000, 10000, depth)
        board.history.clear()
        Position(x, y, this.stone, score)
    }
  }

  def think(board: Board): Position = {
    think(board, depth(board))
  }

  def alphabeta(board: Board, stone: Int, alpha: Int, beta: Int, depth: Int): (Int, Int, Int) = {
    if(depth == 0 || board.checkGameOver) return (-1, -1, evaluate(board, stone))

    val movable = board.calcMovable(stone)
    if(movable.length == 0) {
      board.skip(stone)
      val (_, _, score) = this.alphabeta(board, -stone, -beta, -alpha, depth-1)
      board.unset()
      return (-1, -1, -score)
    }

    var score = alpha
    var bestIndex = (-1, -1)
    for((x, y) <- movable) {
      board.set(x, y, stone)
      val (_, _, score_) = alphabeta(board, -stone, -beta, -score, depth-1)
      board.unset()
      if(-score_ > score) {
        score = -score_
        bestIndex = (x, y)
      }
      if(score >= beta) {
        return (bestIndex._1, bestIndex._2, score)
      }
    }
    (bestIndex._1, bestIndex._2, score)
  }

  def depth(board: Board): Int = 10

  def evaluate(board: Board, stone: Int): Int = board.count(stone) - board.count(-stone)

}

object Player {
  def apply(stone: Int) = new Player(stone)
}
