package net.orfeon.reversi

import org.scalatest.FunSuite

class BoardSuite extends FunSuite {

  test("boardSuite") {
    val board = Board()

    val pos1 = board.calcAcquirables(4, 5, Board.StoneWhite)
    assert(pos1.size == 1)
    assert(pos1(0) == (4, 4))
    val pos2 = board.calcAcquirables(5, 4, Board.StoneWhite)
    assert(pos2.size == 1)
    assert(pos2(0) == (4, 4))

    assert(board.count(Board.StoneBlack) == 2)
    assert(board.count(Board.StoneWhite) == 2)
    assert(board.count(Board.StoneBlank) == 60)
  }

  test("evaluationSuite") {
    val board1 = Board(
      Array[Array[Int]](
        Array[Int](0,1,1,1,1,1,1,-1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](-1,1,1,1,1,1,1,-1)
      )
    )
    // cornerStone
    assert(board1.countCornerStone(Board.StoneBlack) == 3)
    assert(board1.countCornerStone(Board.StoneWhite) == 0)
    // stableStone
    assert(board1.countStableStone(Board.StoneBlack) == 3)
    assert(board1.countStableStone(Board.StoneWhite) == 0)

    val board2 = Board(
      Array[Array[Int]](
        Array[Int](0,1,-1,1,-1,1,-1,-1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1),
        Array[Int](1,1,1,1,1,1,1,1)
      )
    )
    // cornerStone
    assert(board2.countCornerStone(Board.StoneBlack) == 1)
    assert(board2.countCornerStone(Board.StoneWhite) == 2)
    // stableStone
    assert(board2.countStableStone(Board.StoneBlack) == 2)
    assert(board2.countStableStone(Board.StoneWhite) == 20)

  }

}
