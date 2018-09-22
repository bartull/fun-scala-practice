package dogs

object IdMonad {

  type Id[A] = A

  implicit val MonadInstance: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }
}
