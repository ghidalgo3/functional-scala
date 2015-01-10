sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, t : List[A]) extends List[A] {
  def tail : List[A] = t
}

object List {
  def sum(ints : List[Int]) : Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(nums : List[Double]) : Double = {
    nums match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](as : A*) : List[A] = {
    if(as.isEmpty) Nil
    else Cons(as(0), apply(as.tail: _*))
  }
}


val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this one matches
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}