package dogs

import IdMonad.Id
import IdMonad.MonadInstance

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => f(a).asInstanceOf[Id[B]])

  def foldMap[A, B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B = {
    import Traverse._
    type G[X] = Const[B, X]
    traverse(as)(a => f(a).asInstanceOf[G[B]])
  }
}

object Const {
}

object Traverse {

  def apply[F[_]: Traverse]: Traverse[F] = implicitly[Traverse[F]]

  implicit val OptionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa match {
        case Some(a) => implicitly[Applicative[G]].map(f(a))(Some(_))
        case _ => implicitly[Applicative[G]].unit(None)
      }
  }

  implicit val ListTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val ap = implicitly[Applicative[G]]
      fa.foldRight(ap.unit(List.empty[B])) { (a, acc) =>
        ap.map2(f(a), acc)((fa, acc) => fa :: acc)
      }
    }
  }

  private type Const[M, A] = M

  private implicit def constApplicative[M: Monoid]: Applicative[({type F[X] = Const[M, X]})#F] =
    new Applicative[({type F[X] = Const[M, X]})#F] {
      override def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B] =
        implicitly[Monoid[M]].op(fab, fa)

      override def unit[A](a: => A): M =
        implicitly[Monoid[M]].zero
    }
}
