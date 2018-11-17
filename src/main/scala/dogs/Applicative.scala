package dogs

import scala.language.reflectiveCalls

trait Applicative[F[_]] extends Functor[F] with Semigroupal[F] { self =>

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def applyF[A, B](f: F[A => B]): F[A] => F[B] = fa => apply(f)(fa)

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](as: F[A], bs: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(as))(bs)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = {
    val f: A => B => (A, B) = a => b => (a, b)
    apply(lift(f)(ma))(mb)
  }

  def product[G[_]: Applicative]: Applicative[({type H[X] = (F[X], G[X])})#H] =
    new Applicative[({type H[X] = (F[X], G[X])})#H] {
      private val ga = implicitly[Applicative[G]]

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
        val (fapp, gapp) = fab
        val (faa, gaa) = fa
        (self.apply(fapp)(faa), ga.apply(gapp)(gaa))
      }

      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), ga.unit(a))
    }

  def compose[G[_]: Applicative]: Applicative[({type H[X] = F[G[X]]})#H] =
    new Applicative[({type H[X] = F[G[X]]})#H] {
      private val ga = implicitly[Applicative[G]]

      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
        self.map2(fab, fa) { (g1, g2) =>
          ga.apply(g1)(g2)
        }

      override def unit[A](a: => A): F[G[A]] = self.map(self.unit(a))(a => ga.unit(a))
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, acc) =>
      map2(f(a), acc)((a, acc) => a :: acc)
    }

  def sequence[A](as: List[F[A]]): F[List[A]] =
    traverse(as)(identity)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    map(ma)(value => List.fill(n)(value))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(traverse(ms)(a => map(f(a))(if (_) Option(a) else None)))(_.flatten)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { (a, acc) =>
      val (k, fv) = a
      map2(fv, acc)((ffv, aacc) => aacc.updated(k, ffv))
    }
}

object Applicative {

  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit val StreamApplicative: Applicative[Stream] = new Applicative[Stream] {
    override def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] =
      fa.zip(fab).map { case (a, f) => f(a) }

    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
  }

  implicit val OptionApplicative: Applicative[Option] = new Applicative[Option] {
    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      (fab, fa) match {
        case (Some(f), Some(a)) => Some(f(a))
        case _ => None
      }

    override def unit[A](a: => A): Option[A] = Some(a)
  }
}
