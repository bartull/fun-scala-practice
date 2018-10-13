package dogs

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] =
    this match {
      case Halt() => Stream()
      case Emit(h, t) => h #:: t(s)
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
    }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] =
      p match {
        case Halt() => go(this)
        case Emit(h, t) => Emit(h, go(t))
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
      }
    go(this)
  }

  def map[O2](f: O => O2): Process[I, O2] =
    this |> Process.lift(f)

  def ++(p2: Process[I, O]): Process[I, O] =
    this match {
      case Halt() => p2
      case Emit(h, t) => Emit(h, t ++ p2)
      case Await(recv) => Await(i => recv(i) ++ p2)
    }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
    this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(i => recv(i).flatMap(f))
    }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] =
    p2 match {
      case Halt() => Halt()
      case Emit(h2, t2) => Emit(h2, this |> t2)
      case Await(recv2) => this match {
        case Emit(h1, t1) => t1 |> recv2(Some(h1))
        case Await(recv1) => Await(i => recv1(i) |> p2)
        case Halt() => Halt() |> recv2(None)
      }
    }

  def zip[O2](process: Process[I, O2]): Process[I, (O, O2)] =
    flatMap(o => process.map((o, _)))

  def zipWithIndex: Process[I, (O, Int)] =
    this |> Process.loop(0)((a, s) => ((a, s), s + 1))
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {

  implicit def monad[I]: Monad[({type F[O] = Process[I, O]})#F] =
    new Monad[({type F[O] = Process[I, O]})#F] {
      override def flatMap[A, B](fa: Process[I, A])(f: A => Process[I, B]): Process[I, B] = fa.flatMap(f)
      override def unit[A](a: => A): Process[I, A] = Emit(a)
    }

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] =
    liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def take[I](n: Int): Process[I, I] = {
    def go(curr: Int): Process[I, I] =
      if (curr == n) Halt()
      else Await {
        case Some(i) => Emit(i, go(curr + 1))
        case None => Halt()
      }
    go(curr = 0)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(curr: Int): Process[I, I] =
      Await {
        case Some(i) if curr >= n => Emit(i, go(curr))
        case Some(_) => go(curr + 1)
        case _ => Halt()
      }
    go(curr = 0)
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = {
    def go: Process[I, I] =
      Await {
        case Some(i) if p(i) => Emit(i, go)
        case _ => Halt()
      }
    go
  }

  def dropWhile[I](p: I => Boolean): Process[I, I] = {
    def go(drop: Boolean): Process[I, I] =
      Await {
        case Some(i) if drop =>
          val shouldDrop = p(i)
          if (shouldDrop) go(drop = true) else Emit(i, go(drop = false))
        case Some(i) =>
          Emit(i, go(drop = false))
        case _ =>
          Halt()
      }
    go(drop = true)
  }

  def count[I]: Process[I, Int] = loop(0)((_, count) => (count, count + 1))

  def sum: Process[Double, Double] = loop(0.0)((i, sum) => (i + sum, i + sum))

  def mean: Process[Double, Double] = sum.zip(count).map({ case (s, c) => s / c })

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) => f(i, z) match {
        case (o, s2) => Emit(o, loop(s2)(f))
      }
      case None => Halt()
    }

  def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> any

  def existsFirst[I](f: I => Boolean): Process[I, Boolean] =
    Await {
      case Some(i) if f(i) => Emit(true)
      case Some(_) => Emit(false, existsFirst(f))
      case None => Halt()
    }

  def any: Process[Boolean, Boolean] = loop(false)((a, s) => (a || s, a || s))
}
