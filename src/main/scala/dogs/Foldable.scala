package dogs

trait Foldable[F[_]] {

  def foldMap[A, B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)((a: A) => (b: B) => f(b, a))(Monoid.dual(Monoid.endoMonoid))(z)

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])((a, acc) => a :: acc)
}

object Foldable {

  def apply[F[_]: Foldable]: Foldable[F] = implicitly[Foldable[F]]

  implicit val ListFoldable: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(implicit mb: Monoid[B]): B =
      as.map(f).fold(mb.zero)(mb.op)
  }

  implicit val StreamFoldable: Foldable[Stream] = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: A => B)(implicit mb: Monoid[B]): B =
      as.map(f).fold(mb.zero)(mb.op)
  }

  implicit val IndexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit mb: Monoid[B]): B =
      as.map(f).fold(mb.zero)(mb.op)
  }

  implicit val OptionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(implicit mb: Monoid[B]): B =
      as.map(f).getOrElse(mb.zero)
  }
}
