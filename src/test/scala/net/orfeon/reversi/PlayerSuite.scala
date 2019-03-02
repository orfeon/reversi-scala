package net.orfeon.reversi

import org.scalatest.FunSuite

class PlayerSuite extends FunSuite {
  test("thinkSuite") {
    val board1 = Board(
      Array[Array[Int]](
        Array[Int](0,0,0,0,0,0,0,0),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](-1,1,1,1,1,1,1,1),
        Array[Int](-1,-1,-1,-1,-1,-1,-1,-1)
      )
    )
    val player = Player(Board.StoneBlack)
    val pos1 = player.think(board1, 1)
    assert((pos1.x, pos1.y) == (0, 7))

    val board2 = Board(
      Array[Array[Int]](
        Array[Int](0,0,0,0,0,0,0,0),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,-1,1,1,1,1,1,1),
        Array[Int](-1,-1,-1,-1,-1,-1,-1,-1)
      )
    )
    val pos2 = player.think(board2, 1)
    assert((pos2.x, pos2.y) == (0, 0))
    assert(pos2.score == -13)
    board2.set(pos2)
    board2.show
  }
}
