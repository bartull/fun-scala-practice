package dogs

import scala.concurrent.{ExecutionContext, Future}

trait Monad[F[_]] extends Applicative[F] { self =>

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fa)(a => map(fab)(f => f(a)))

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => flatMap(f(a))(b => g(b))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def composeM[G[_]: Monad: Traverse]: Monad[({type H[X] = F[G[X]]})#H] =
    new Monad[({type H[X] = F[G[X]]})#H] {
      private val gm = implicitly[Monad[G]]
      private val gt = implicitly[Traverse[G]]

      override def unit[A](a: => A): F[G[A]] = self.unit(gm.unit(a))

      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fa) { ga =>
          self.map(gt.traverse(ga)(a => f(a))(self))(s => gm.join(s))
        }
    }
}

object Monad {

  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  implicit val ListMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  implicit def futureMonad(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
    override def unit[A](a: => A): Future[A] = Future.successful(a)
  }

  implicit def eitherMonad[E]: Monad[({ type F[X] = Either[E, X]})#F] =
    new Monad[({ type F[X] = Either[E, X]})#F] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
    }
}
