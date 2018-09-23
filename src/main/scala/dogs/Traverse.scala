package dogs

import IdMonad.Id
import IdMonad.MonadInstance

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

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

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, list) => (list.head, list.tail))._1

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, idx) => ((a, idx), idx + 1))._1

  def mapAccum[A, B, S](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverse(fa) { a =>
      for {
        curr <- State.get[S]
        (b, next) = f(a, curr)
      } yield b
    }.run(s).swap

  def fuse[G[_], H[_]: Applicative, A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                                         (implicit ga: Applicative[G]): (G[F[B]], H[F[B]]) =
    traverse[({type F[X] = (G[X], H[X])})#F, A, B](fa)(a => (f(a), g(a)))(ga.product[H])

  def compose[G[_]](implicit g: Traverse[G]): Traverse[({type H[X] = F[G[X]]})#H] =
    new Traverse[({type H[X] = F[G[X]]})#H] {
      override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: A => M[B]): M[F[G[B]]] =
        self.traverse(fa)(ga => g.traverse(ga)(f))
    }
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
