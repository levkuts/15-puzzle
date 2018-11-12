package ua.levkuts.puzzle

import scalaz.Scalaz._
import scalaz.effect.IO._
import scalaz.effect._
import ua.levkuts.puzzle.Board.BoardMonadT

import scala.language.higherKinds
import scala.util.Random

object Main extends App {
  type BoardMonadTIO[A] = BoardMonadT[IO, A]

  private val rng = new Random(System.currentTimeMillis)

  val greeting: IO[Unit] = for {
    _ <- putStrLn("Who are your?")
    name <- readLn
    _ <- putStrLn(s"Hola $name, let's start our game")
    _ <- putStrLn("You can move left (l), right (r), up (u) and down (d)")
  } yield ()

  val Commands = Map(
    "l" -> Left,
    "r" -> Right,
    "u" -> Up,
    "d" -> Down
  )

  val getCommandString: IO[Option[Command]] = for {
    _ <- putStrLn("Your turn: ")
    cmdStr <- readLn
  } yield Commands.get(cmdStr)

  def getCommand(getCommandString: IO[Option[Command]]): IO[Command] =
    getCommandString.flatMap {
      case Some(cmd) => cmd.point[IO]
      case None => for {
        _ <- putStrLn("You're really tricky bastard, don't try my patience")
        res <- getCommand(getCommandString)
      } yield res
    }

  val processCommand: BoardMonadTIO[GameState] = for {
    _ <- putStrLn("Board: ").liftM[BoardMonadT]
    boardStr <- gets { b: Board => b.toString }.lift[IO]
    _ <- putStrLn(boardStr).liftM[BoardMonadT]
    cmd <- getCommand(getCommandString).liftM[BoardMonadT]
    _ <- modify { b => Game.processCommand(cmd)(b) }.lift[IO]
    s <- gets { b => Game.getState(b) }.lift[IO]
  } yield s

  def checkGameState(state: BoardMonadTIO[GameState]): BoardMonadTIO[Unit] =
    state.flatMap { s =>
      s match {
        case Struggling => checkGameState(state).map(_ => Unit)
        case Victory => putStrLn("Congratulations, you won!").liftM[BoardMonadT]
      }
    }

  val initBoard = Game.init(complexity = 10, rng = rng)
  val playGame = checkGameState(processCommand).run(initBoard).map(_._2)
  (greeting |+| playGame).unsafePerformIO()
}
