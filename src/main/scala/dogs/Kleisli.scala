package dogs

case class Kleisli[F[_]: Monad, A, B](run: A => F[B]) {
  def andThen[C](k1: Kleisli[F, B, C]): Kleisli[F, A, C] = Kleisli { a =>
    Monad[F].flatMap(run(a))(b => k1.run(b))
  }

  def flatMap[C](f: B => Kleisli[F, A, C]): Kleisli[F, A, C] = Kleisli { a =>
    Monad[F].flatMap(run(a))(b => f(b).run(a))
  }
}

object Kleisli {

  def unit[F[_]: Monad, A, B](b: B): Kleisli[F, A, B] =
    Kleisli[F, A, B](_ => Monad[F].unit(b))

  def monad[F[_]: Monad, C]: Monad[({type K[B] = Kleisli[F, C, B]})#K] =
    new Monad[({type K[B] = Kleisli[F, C, B]})#K] {

      override def flatMap[A, B](fa: Kleisli[F, C, A])(f: A => Kleisli[F, C, B]): Kleisli[F, C, B] =
        fa.flatMap(f)

      override def unit[B](b: => B): Kleisli[F, C, B] = Kleisli.unit[F, C, B](b)
    }
}
