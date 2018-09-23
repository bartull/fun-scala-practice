package dogs

case class State[S, +A](run: S => (S, A)) { self =>

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { state =>
    val (nextState, a) = self.run(state)
    f(a).run(nextState)
  }
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (s, a))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](state: S): State[S, Unit] = State(_ => (state, ()))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      curr <- get
      _ <- set(f(curr))
    } yield ()

  implicit def stateMonad[S]: Monad[({type F[X] = State[S, X]})#F] =
    new Monad[({type F[X] = State[S, X]})#F] {
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
      override def unit[A](a: => A): State[S, A] = unit(a)
    }
}
