package dogs

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](value: A) extends Validation[Nothing, A]

object Validation {
  implicit def applicative[E]: Applicative[({ type F[X] = Validation[E, X]})#F] =
    new Applicative[({ type F[X] = Validation[E, X]})#F] {
      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        (fab, fa) match {
          case (Success(f), Success(a)) => Success(f(a))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, h2 +: t1 ++: t2)
          case (f1@Failure(_, _), _) => f1
          case (_, f2@Failure(_, _)) => f2
        }

      override def unit[A](a: => A): Validation[E, A] = Success(a)
    }
}
