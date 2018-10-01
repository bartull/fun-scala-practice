package examples

import dogs.Free.Suspend
import dogs.Translate.~>
import dogs.{Free, Monad, State, TailRec}
import examples.Console.ConsoleState

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.Try

sealed trait Console[A] {
  def toFuture: Future[A]
  def toThunk: () => A
  def toState: ConsoleState[A]
}

case class Buffers(in: List[String], out: List[String])

object Console {

  type ConsoleIO[A] = Free[Console, A]
  type ConsoleState[A] = State[Buffers, A]

  case object ReadLine extends Console[Option[String]] {
    override def toFuture: Future[Option[String]] = Future(run())
    override def toThunk: () => Option[String] = () => run()
    override def toState: ConsoleState[Option[String]] =
      State((buffers: Buffers) => (Buffers("Test" :: buffers.in, buffers.out), Some("Test")))

    def run(): Option[String] = Try(readLine()).toOption
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toFuture: Future[Unit] = Future(run())
    override def toThunk: () => Unit = () => run()
    override def toState: ConsoleState[Unit] =
      State((buffers: Buffers) => (Buffers(buffers.in, line :: buffers.out), Unit))

    def run(): Unit = println(line)
  }

  val consoleToFuture: Console ~> Future = new (Console ~> Future) {
    override def apply[A](f: Console[A]): Future[A] = f.toFuture
  }

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToState: Console ~> ConsoleState = new (Console ~> ConsoleState) {
    override def apply[A](f: Console[A]): ConsoleState[A] = f.toState
  }

  def runThunk[A](io: ConsoleIO[A]): A =
    // We could use Free.runFree here, but Monad for Function0 is not stack safe.
    // We translate Free[Console, A] to Free[Function0, A] and use the specialized TailRec!
    TailRec.run(Free.translate(io)(consoleToFunction0))

  def runFuture[A](io: ConsoleIO[A]): Future[A] =
    Free.runFree(io)(consoleToFuture)

  def runState[A](io: ConsoleIO[A]): ConsoleState[A] =
    Free.runFree(io)(consoleToState)

  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))
}

object ConsoleApp extends App {
  import Console._

  val action =
    for {
      _ <- printLn("Hello from the free world! What is your name?")
      name <- readLn
      _ <- printLn(s"Hello, ${name.getOrElse("Stranger")}!")
    } yield ()

  println(runState(action).run(Buffers(List(), List())))
}
