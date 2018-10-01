package dogs

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]

  def identity[F[_]]: F ~> F = new (F ~> F) {
    override def apply[A](f: F[A]): F[A] = f
  }
}
