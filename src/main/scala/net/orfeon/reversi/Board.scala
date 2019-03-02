package net.orfeon.reversi

class Board(val stones: Array[Array[Int]], val history: History, val liberty: Array[Array[Int]]) {

  def set(pos: Position): Int = {
    set(pos.x, pos.y, pos.stone)
  }

  def set(x: Int, y: Int, stone: Int): Int = {
    if(!(x, y).inboard) {
      this.history.skip(stone)
      return 0
    }
    val acquirables = this.calcAcquirables(x, y, stone)
    if(acquirables.length == 0) {
      this.history.skip(stone)
      return 0
    }

    acquirables.foreach(xy => this.stones(xy._1)(xy._2) = stone)
    this.stones(x)(y) = stone
    this.history.push(Position(x, y, stone, 0, acquirables))

    // Calculate liberty score
    Board.Directions
      .map(dir => (x, y) + dir)
      .filter(_.inboard)
      .foreach(xy => this.liberty(xy._1)(xy._2) -= 1)

    return acquirables.length + 1
  }

  def unset(): Unit = {
    if(this.history.size == 0) return

    val pos = this.history.pop
    if(pos.skip) return

    val stone = this.stones(pos.x)(pos.y)
    pos.acquirables.foreach(index => this.stones(index._1)(index._2) = -stone)
    this.stones(pos.x)(pos.y) = Board.StoneBlank

    // Calculate liberty score
    Board.Directions
      .map(dir => (pos.x, pos.y) + dir)
      .filter(_.inboard)
      .foreach(xy => this.liberty(xy._1)(xy._2) += 1)
  }

  def skip(stone: Int): Unit = this.history.push(Position(stone))

  def turn: Int = -this.history.last.stone

  def calcMovable(stone: Int): Array[(Int,Int)] = {
    return (for(x <- 0 until 8; y <- 0 until 8) yield (x, y, this.calcAcquirables(x, y, stone).length))
      .filter(_._3 > 0)
      .map(s => (s._1, s._2)).toArray
  }

  def calcAcquirables(x: Int, y: Int, stone: Int): Seq[(Int,Int)] = {
    if(this.stones(x)(y) != Board.StoneBlank) Nil
    else Board.Directions
        .map(dir => calcAcquirablesDir(stone, (x + dir._1, y + dir._2), dir, Nil))
        .fold(Nil)(_ ++ _)
  }

  private[this] def calcAcquirablesDir(stone: Int, index: (Int,Int), dir: (Int,Int), acc: Seq[(Int, Int)]): Seq[(Int, Int)] =
    if(!index.inboard) Nil
    else this.stones.stone(index) match {
      case n if n ==  stone => acc
      case n if n == -stone => calcAcquirablesDir(stone, index + dir, dir, acc :+ index)
      case _ => Nil
    }

  def count(stone: Int): Int = this.stones.foldLeft(0)((a,b) => a + b.count(_ == stone))

  def checkGameOver: Boolean = this.count(Board.StoneBlank) == 0 || this.history.checkLastSkipNum() == 2

  def clear(): Unit = {
    for(x <- 0 until 8; y <- 0 until 8) {
      this.stones(x)(y) = (x, y) match {
        case (3, 3) | (4, 4) => Board.StoneBlack
        case (3, 4) | (4, 3) => Board.StoneWhite
        case _ => Board.StoneBlank
      }
      this.liberty(x)(y) = 0
    }
    this.history.clear()
  }

  // Functions for board evaluation
  def countMobility(stone: Int): Int = this.calcMovable(stone).length

  def countLiverty(stone: Int): Int = this.liberty.map(row => row.sum).sum

  def countCornerStone(stone: Int): Int = stone * Board.CornerPositions.map(this.stones.stone(_)).filter(_ == stone).sum

  def countStableStone(stone: Int): Int = {
    def countLineStableStone(index: (Int, Int), dir: (Int,Int), count: Int): Int = {
      val nextIndex = index + dir
      if (nextIndex.inboard && stones.stone(nextIndex) == stone) countLineStableStone(nextIndex, dir, count + 1)
      else count
    }
    Board.CornerPositions.filter(stones.stone(_) == stone).flatMap{_ match {
      case i if i == (0, 0) => Seq(countLineStableStone(i, (0, 1), 1), countLineStableStone(i, (1, 0), 1))
      case i if i == (0, 7) => Seq(countLineStableStone(i, (1, 0), 1), countLineStableStone(i, (0, -1), 1))
      case i if i == (7, 0) => Seq(countLineStableStone(i, (0, 1), 1), countLineStableStone(i, (-1, 0), 1))
      case i if i == (7, 7) => Seq(countLineStableStone(i, (-1, 0), 1), countLineStableStone(i, (0, -1), 1))
      case _ => Nil
    }}.map(n => (if(n == 8) 4.0 else n) - 0.5).sum.toInt
  }

  // for debug
  def show: Unit = this.stones.foreach(e => {
    e.foreach(l => print(if(l<0) "o" else if(l>0) "x" else " "))
    println("")
  })

  implicit class Point(val self: (Int, Int)) {
    def +(that: (Int, Int)): (Int, Int) = (self._1 + that._1, self._2 + that._2)
    def *(times: Int): (Int, Int) = (self._1 * times, self._2 * times)
    def inboard = self._1 >= 0 && self._1 < 8 && self._2 >= 0 && self._2 < 8
  }

  implicit class Stones(val self: Array[Array[Int]]) {
    def stone(index: (Int,Int)) = self(index._1)(index._2)
  }

}

object Board {
  val StoneBlack: Int = -1
  val StoneWhite: Int = 1
  val StoneBlank: Int = 0
  // right, left, up, down, right-up, right-down, left-up, left-down
  val Directions: Array[(Int, Int)] = Array((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
  val CornerPositions: Array[(Int, Int)] = Array((0, 0), (0, 7), (7, 0), (7, 7))

  def apply(): Board = {
    val board = new Board(Array.ofDim[Int](8,8), History(new Array[Position](60), 0), Array.ofDim[Int](8,8))
    board.clear()
    board
  }
  def apply(stones: Array[Array[Int]]): Board = new Board(stones, History(new Array[Position](60), 0), Array.ofDim[Int](8,8))
  def apply(stones: Array[Array[Int]], history: History, liberty: Array[Array[Int]]): Board = new Board(stones, history,liberty)
}

case class Position(val x: Int, val y: Int, val stone: Int, val score: Int, val skip: Boolean, val acquirables: Seq[(Int,Int)])

object Position {
  def apply(x: Int, y: Int, stone: Int, score: Short, acquirables: Seq[(Int,Int)]): Position = new Position(x, y, stone, score, false, acquirables)
  def apply(x: Int, y: Int, stone: Int, score: Int): Position = new Position(x, y, stone, score, false, Nil)
  def apply(stone: Int) = new Position(-1, -1, stone, 0, true, Nil)
}

class History(val positions: Array[Position], var lastIndex: Int) {

  def push(position: Position): Unit = {
    this.positions(this.lastIndex) = position
    this.lastIndex += 1
  }

  def pop(): Position = {
    if(this.lastIndex == 0) throw new RuntimeException("Index")
    this.lastIndex -= 1
    this.positions(this.lastIndex)
  }

  def skip(stone: Int): Unit = push(Position(stone))

  def last: Position = {
    if(this.lastIndex == 0) throw new RuntimeException("Index ")
    this.positions(this.lastIndex)
  }

  def size: Int = this.lastIndex

  def clear(): Unit = this.lastIndex = 0

  def checkLastSkipNum(): Int = {
    return {
      (if(this.lastIndex > 0 && this.positions(this.lastIndex - 1).skip) 1 else 0) +
      (if(this.lastIndex > 1 && this.positions(this.lastIndex - 2).skip) 1 else 0)
    }
  }

}

object History {
  def apply(positions: Array[Position], lastIndex: Int): History = new History(positions, lastIndex)
}