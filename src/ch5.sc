sealed trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }
  }

  def take(n : Int) : Stream[A] = {
    n match {
      case 0 => Empty
      case _ => this match {
        case Cons(h, t) => Stream.cons(h(), t().take(n-1))
        case _ => Empty
      }
    }
  }

  def drop(n : Int) : Stream[A] = {
    @annotation.tailrec
    def dropHelp(m : Int, accum: Stream[A]): Stream[A] = {
      m match {
        case 0 => accum
        case _ => accum match {
          case Empty => Empty
          case Cons(h, t) => dropHelp(m-1, t())
        }
      }
    }
    dropHelp(n, this)
  }

  def takeWhile(p : A => Boolean) : Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }
  }

  def foldRight[B](z : => B)(f: (A, => B) => B) : B = {
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }
  }

  def map[B](m : A => B) : Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => Cons(() => m(a), () => b))
  }

  def flatMap[B](f : A => Stream[B]) : Stream[B] = {
    foldRight(Stream.empty[B])((a,b) => f(a) append b)
  }

  def filter(p : A => Boolean) : Stream[A] = {
    foldRight(Empty : Stream[A])((a, b) => if(p(a)) Stream.cons(a,b) else b)
  }

  def append[B >: A](more : => Stream[B]) : Stream[B] = {
    foldRight(more)((a,b) => Stream.cons(a,b))
  }

  def headOption : Option[A] = {
    this match {
      case Cons(h,_) => Some(h())
      case _ => None
    }
  }

  def find(p : A => Boolean) : Option[A] = {
    filter(p).headOption
  }

  def takeWhileFold(p : A => Boolean) : Stream[A] = {
    //silly compiler not inferring types
    foldRight(Empty : Stream[A])((a, b) => if(p(a)) Cons(() => a,() => b) else Empty)
  }

  def forAll(p : A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => if(p(h())) t().forAll(p)  else false
      case _ => true
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

  def empty[A] : Stream[A] = Empty

  def apply[A](as : A*) : Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail:_*))
  }

  def constant[A](a : A) : Stream[A] = {
    lazy val stream : Stream[A] = cons(a,stream)
    stream
  }
  def from(n : Int) : Stream[Int] = {
    val stream : Stream[Int] = cons(n, from(n + 1))
    stream
  }

  def unfold[A,S](z: S)(f : S => Option[(A,S)]) : Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty[A]
    }
  }


//  def fib : Stream[Int] = {
//    lazy val stream : Stream[Int] = cons(0, fib).foldRight(0)((a,b) => )
//  }
}
Stream(1,2,3,4,5,6).append(Stream(12,3)).toList.headOption
val ones : Stream[Int] = Stream.cons(1, ones)
Stream.unfold(0)(s => if (s < 10) Option((s + 1, s+1)) else None).toList
