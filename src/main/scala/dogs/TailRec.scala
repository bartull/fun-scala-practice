package dogs

import dogs.Free.{FlatMap, Return, Suspend}

import scala.annotation.tailrec

// Non-stack safe Monad can "blow" in two scenarios:
// 1. Interpretation (run)
// 2. Kleisli composition (without tricks like Suspend)
object TailRec {

  type TailRec[A] = Free[Function0, A]

  def apply[A](a: => A): TailRec[A] = unit(a)

  def unit[A](a: => A): TailRec[A] = Return(a)

  // Trampolining to avoid a stack overflow.
  // Uses the monad associativity law, transforms a chain of ADTs, utilizes heap instead of stack.
  @tailrec
  def run[A](io: TailRec[A]): A =
    io match {
      case Return(a) => a
      case Suspend(resume) => resume()
      case FlatMap(sub1, f1) => sub1 match {
        case Return(a) => run(f1(a))
        case Suspend(resume) => run(f1(resume()))
        case FlatMap(sub2, f2) =>
          // Initial:
          // - ((sub2).flatMap(f2)).flatMap(f1)
          // - FlatMap(FlatMap(sub2, f2), f1)
          // Transformed to:
          // - sub2.flatMap(a => f2(a).flatMap(f1))
          // - FlatMap(sub2, a => FlatMap(f2(a), f1))
          run(sub2.flatMap(a => f2(a).flatMap(f1)))
      }
    }
}

private object TailRecExample {
  import TailRec._

  val f: Int => TailRec[Int] = x => Return(x)

  val blowsWhenCalled: Int => TailRec[Int] =
    List.fill(1000000)(f).foldLeft(f) { (acc, next) =>
      // Equivalent to the one below (huge function calls chain)
      // x => acc(x).flatMap(next)
      Monad[TailRec].compose(acc, next)
    }

  val safeWhenCalled: Int => TailRec[Int] =
    List.fill(1000000)(f).foldLeft(f) { (acc, next) =>
      // Equivalent to the one below (suspend breaks calls chain)
      // x => Suspend(() => ()).flatMap(_ => acc(x)).flatMap(next)
      Monad[TailRec].compose(x => Suspend(() => ()).flatMap(_ => acc(x)), next)
    }
}
