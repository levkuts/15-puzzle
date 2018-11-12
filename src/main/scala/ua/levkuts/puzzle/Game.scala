package ua.levkuts.puzzle

import scalaz.{State, StateT}

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps}
import scala.util.Random

sealed trait Command

case object Left extends Command

case object Right extends Command

case object Up extends Command

case object Down extends Command

case class Point(x: Int, y: Int) {
  def incX: Point = copy(x + 1, y)

  def decX: Point = copy(x - 1, y)

  def incY: Point = copy(x, y + 1)

  def decY: Point = copy(x, y - 1)
}

case class Board(numbers: Array[Int], position: Point) {

  import Board._

  def isPossiblePosition(position: Point): Boolean =
    position.x >= 0 && position.x < Width &&
      position.y >= 0 && position.y < Width

  def move(newPosition: Point): Board =
    copy(numbers = swap(numbers.clone(), pointToIndex(position), pointToIndex(newPosition)), position = newPosition)

  private def swap(arr: Array[Int], idx1: Int, idx2: Int): Array[Int] = {
    val tmp = arr(idx1)
    arr(idx1) = arr(idx2)
    arr(idx2) = tmp
    arr
  }

  override def toString: String =
    numbers.map(i => if (i == 0) "" else i.toString).grouped(Board.Width)
      .toArray.map(_.toArray).map(_.mkString("\t")).mkString("\n")

  override def equals(other: Any): Boolean = other match {
    case b: Board => numbers.deep == b.numbers.deep && position == b.position
    case _ => false
  }
}

object Board {
  val Width: Int = 4
  val Size: Int = Width * Width

  type BoardMonad[A] = State[Board, A]
  type BoardMonadT[M[_], A] = StateT[M, Board, A]

  def apply(numbers: Array[Int]): Board = new Board(numbers, indexToPoint(numbers.indexOf(0)))

  def indexToPoint(idx: Int): Point = Point(idx % Width, idx / Width)

  def pointToIndex(p: Point): Int = p.y * Width + p.x
}

sealed trait GameState

case object Struggling extends GameState

case object Victory extends GameState

object Game {

  def getNewPosition(position: Point, cmd: Command): Point =
    cmd match {
      case Left => position.decX
      case Right => position.incX
      case Up => position.decY
      case Down => position.incY
    }

  def processCommand(cmd: Command)(board: Board): Board = {
    val newPosition = getNewPosition(board.position, cmd)
    if (board.isPossiblePosition(newPosition)) board.move(newPosition) else board
  }

  private def isSorted[T](a: Array[T])(implicit ord: Ordering[T]): Boolean =
    !a.zip(a.tail).exists { case (x, y) => ord.gt(x, y) }

  def getState(b: Board): GameState = if (isSorted(b.numbers.init)) Victory else Struggling

  def getRandomCommand(rng: Random): Command =
    rng.nextInt(4) match {
      case 0 => Left
      case 1 => Right
      case 2 => Up
      case 3 => Down
    }

  @tailrec
  def shuffle(board: Board, movesCount: Int, rng: Random): Board = {
    if (movesCount == 0) board
    else {
      val cmd = getRandomCommand(rng)
      val newPosition = getNewPosition(board.position, cmd)
      if (board.isPossiblePosition(newPosition)) shuffle(board.move(newPosition), movesCount - 1, rng)
      else shuffle(board, movesCount, rng)
    }
  }

  def init(complexity: Int, rng: Random): Board =
    shuffle(Board((1 to 15 toArray) :+ 0), complexity * 10, rng)

}
