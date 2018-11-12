package ua.levkuts.puzzle

import org.mockito.Mockito._
import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import scala.language.postfixOps
import scala.util.Random

class GameTest extends WordSpec with MockitoSugar {
  "Game" should {
    "calculate new position based on the command" in {
      val pos = Point(2, 2)

      assert(Game.getNewPosition(pos, Left) == Point(1, 2))
      assert(Game.getNewPosition(pos, Right) == Point(3, 2))
      assert(Game.getNewPosition(pos, Up) == Point(2, 1))
      assert(Game.getNewPosition(pos, Down) == Point(2, 3))
    }

    "move hole on acceptable command" in {
      val expectedBoard = Board((1 to 15 toArray) :+ 0)
      val initBoard = Board((1 to 14 toArray) :+ 0 :+ 15)

      val actualBoard = Game.processCommand(Right)(initBoard)

      assert(actualBoard == expectedBoard)
    }

    "not move hole on unacceptable command" in {
      val initBoard = Board((1 to 15 toArray) :+ 0)

      val actualBoard = Game.processCommand(Right)(initBoard)

      assert(actualBoard == initBoard)
    }

    "return victory state when the game is finished" in {
      assert(Game.getState(Board((1 to 15 toArray) :+ 0)) == Victory)
    }

    "return struggling state while the game is in progress" in {
      assert(Game.getState(Board((1 to 14 toArray) :+ 0 :+ 15)) == Struggling)
    }

    "return random command" in {
      val rng = mock[Random]
      when(rng.nextInt(org.mockito.Matchers.any[Int])) thenReturn 2 thenReturn 0 thenReturn 3

      assert(Game.getRandomCommand(rng) == Up)
      assert(Game.getRandomCommand(rng) == Left)
      assert(Game.getRandomCommand(rng) == Down)
    }

    "shuffle board" in {
      val initBoard = Board((1 to 15 toArray) :+ 0)
      val movesCount = 5
      val rng = mock[Random]
      when(rng.nextInt(org.mockito.Matchers.any[Int])).thenReturn(0).thenReturn(0)
        .thenReturn(2) thenReturn 1 thenReturn 2
      val expectedBoard = Board((1 to 6 toArray) :+ 0 :+ 8 :+ 9 :+ 11 :+ 7 :+ 12 :+ 13 :+ 10 :+ 14 :+ 15)

      val board = Game.shuffle(initBoard, movesCount, rng)

      assert(expectedBoard == expectedBoard)
    }
  }
}
