package dogs

import dogs.Free.Return

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Async {

  type Async[A] = Free[Future, A]

  def apply[A](a: => A): Async[A] = unit(a)

  def unit[A](a: => A): Async[A] = Return(a)

  def run[A](async: Async[A]): Future[A] = Free.run(async)
}
