package dogs

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[({type F[X] = Reader[R, X]})#F] =
    new Monad[({type F[X] = Reader[R, X]})#F] {
      def unit[A](a: => A): Reader[R, A] =
        Reader(_ => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader[R, B](r => f(st.run(r)).run(r))
    }
}
