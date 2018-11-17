package dogs

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {

  implicit val StringMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""
    override def op(a1: String, a2: String): String = a1 + a2
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def zero: List[A] = List.empty[A]
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  implicit def optionMonoid[A](implicit a: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  }

  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = a => a
    override def op(a1: A => A, a2: A => A): A => A = a => a1(a2(a))
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def zero: A = m.zero
    override def op(a1: A, a2: A): A = m.op(a2, a1)
  }

  val IntAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val IntMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1
    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val BooleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val BooleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
    as.foldLeft(m.zero)((b1, a2) => m.op(b1, f(a2)))

  def foldMapV[A, B](v: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = {
    if (v.isEmpty) m.zero
    else v match {
      case IndexedSeq(v1) =>
        f(v1)
      case _ =>
        val (v1, v2) = v.splitAt(v.size / 2)
        m.op(foldMapV(v1)(f), foldMapV(v2)(f))
    }
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (a.zero, b.zero)
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => b.zero
    override def op(a1: A => B, a2: A => B): A => B = a =>
      b.op(a1(a), a2(a))
  }

  def mapMergeMonoid[K, V](v: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map.empty[K, V]
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(Map.empty[K, V]) { (acc, key) =>
        val v1 = a1.getOrElse(key, v.zero)
        val v2 = a2.getOrElse(key, v.zero)
        acc.updated(key, v.op(v1, v2))
      }
  }
}
