package dogs

import dogs.Translate.~>

import scala.annotation.tailrec

sealed trait Free[F[_], A] { self =>
  import Free._

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(self, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Free.Return(f(a)))
}

object Free {

  case class Return[F[_], A](what: A) extends Free[F, A]
  case class Suspend[F[_], A](f: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](what: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def apply[F[_], A](a: A): Free[F, A] = monad[F].unit(a)

  implicit def monad[F[_]]: Monad[({type X[A] = Free[F, A]})#X] =
    new Monad[({type X[A] = Free[F, A]})#X] {
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
      override def unit[A](a: => A): Free[F, A] = Return(a)
    }

  @tailrec
  private def step[F[_], A](free: Free[F, A]): Free[F, A] =
    free match {
      case FlatMap(FlatMap(sub2, f2), f1) => step(sub2.flatMap(a => f2(a).flatMap(f1)))
      case FlatMap(Return(what), f1) => step(f1(what))
      case _ => free
    }

  def run[F[_]: Monad, A](free: Free[F, A]): F[A] =
    runFree(free)(Translate.identity[F])

  def runFree[F[_], G[_]: Monad, A](free: Free[F, A])(t: F ~> G): G[A] =
    step(free) match {
      case Return(what) => implicitly[Monad[G]].unit(what)
      case Suspend(f) => t(f)
      case FlatMap(Suspend(f2), f1) => implicitly[Monad[G]].flatMap(t(f2))(a => runFree(f1(a))(t))
      case _ => sys.error("Impossible (covered by step)")
    }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[B] = Free[G, B]
    val translateFreeG: F ~> FreeG = new (F ~> FreeG) {
      override def apply[C](f: F[C]): FreeG[C] = Suspend(fg(f))
    }
    runFree(f)(translateFreeG)
  }
}
