//don't know what is going on here

sealed trait Stream[A] {
  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }
  }

  def take(n : Int) : Stream[A] = {
    n match {
      case 0 => Empty[A]
      case _ => this match {
        case Cons(h, t) => Stream.cons(h(), take(n-1))
        case _ =>
      }
    }
  }

  def drop(n : Int) : Stream[A] = {
    def dropHelp(m : Int, accum: Stream[A]): Stream[A] = {
      m match {
        case 0 => accum
        case _ => accum match {
          case Cons(h, t) => t()
          case _ => Empty[A]
        }
      }
    }
    dropHelp(n, this)
  }

  def takeWhile(p : A => Boolean) : Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => takeWhile(p))
      case _ => Empty[A]
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty[A]

  def apply[A](as : A*) : Stream[A] = {
    if (as.isEmpty) Empty[A] else cons(as.head, apply(as.tail:_*))
  }
}

Stream(1,2,3).toList

