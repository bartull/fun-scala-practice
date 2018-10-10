package dogs

sealed trait StateTag[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): StateTag[S, B] = new StateTag[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, ns) = self.run(s)
      (f(a), ns)
    }
  }

  def flatMap[B](f: A => StateTag[S, B]): StateTag[S, B] = new StateTag[S, B] {
    override protected def run(s: S): (B, S) = {
      val (a, ns) = self.run(s)
      f(a).run(ns)
    }
  }
}

object StateTag {
  def apply[S, A](a: => A): StateTag[S, A] = unit(a)

  def unit[S, A](a: => A): StateTag[S, A] = new StateTag[S, A] {
    private lazy val memo = a
    override protected def run(s: S): (A, S) = (memo, s)
  }

  def run[A](what: RunnableStateTag[A]): A =
    what.apply[Unit].run(())._1
}

sealed trait StateTagRef[S, A] {

  protected var cell: A

  def read: StateTag[S, A] = StateTag(cell)

  def write(a: A): StateTag[S, Unit] = new StateTag[S, Unit] {
    override protected def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object StateTagRef {
  def apply[S, A](a: A): StateTag[S, StateTagRef[S, A]] = unit(a)

  def unit[S, A](a: A): StateTag[S, StateTagRef[S, A]] =
    StateTag(
      new StateTagRef[S, A] {
        override protected var cell: A = a
      }
    )
}

sealed trait RunnableStateTag[A] {
  def apply[S]: StateTag[S, A]
}

object StateTagApp extends App {

  val example: RunnableStateTag[Int] = new RunnableStateTag[Int] {
    def apply[S]: StateTag[S, Int] =
      for {
        r1 <- StateTagRef(1)
        r2 <- StateTagRef(1)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(x + 1)
        _ <- r2.write(y + 1)
        a <- r1.read
        b <- r2.read
      } yield a + b
  }

  println(StateTag.run(example))
}
