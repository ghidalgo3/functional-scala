sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, t : List[A]) extends List[A] {
  def tail : List[A] = t
}
object List {

  //3.7 halting product
  //I don't think its possible because the operation f() is only
  //applied when the recursion terminates so foldRight cannot terminate
  //early
  def sum(ints : List[Int]) : Int = {
    foldRight(ints, 0)(_ + _)
  }
  def product(nums : List[Double]) : Double = {
    foldRight(nums, 1.0)(_ * _)
  }
  def apply[A](as : A*) : List[A] = {
    if(as.isEmpty) Nil
    else Cons(as(0), apply(as.tail: _*))
  }
  //3.9 length
  def length[A](list : List[A]) : Int = {
    foldRight(list, 0)((_,c) => c + 1)
  }

  def foldRight[A,B](list : List[A], z : B)(f : (A,B) => B) : B = {
    list match {
      case Nil => z
      case Cons(a, tail) => f(a, foldRight(tail, z)(f))
    }
  }

  def leftSum(list : List[Int]) : Int = {
    foldLeft(list,0)(_ + _)
  }

  def leftProduct(list : List[Double]) : Double = {
    foldLeft(list, 1.0)(_ * _)
  }

  def leftLength[A](list : List[A]) : Int = {
    foldLeft(list, 0)((c, _) => c + 1)
  }

  def reverse[A](list : List[A]) : List[A] = {  //this was hard!
    foldLeft(list, Nil: List[A])((a, b) => Cons(b, a)) // A, B => B
  }

  //fold right in terms of fold left
  def leftFoldRight[A,B](list : List[A], z : B)(f : (A,B) => B) : B = {
    foldLeft(reverse(list), z)((a,b) => f(b,a))
  }

  @annotation.tailrec
  def foldLeft[A,B](list: List[A], z : B)(f : (B,A) => B) : B = {
    list match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def flatten[A](bigList : List[List[A]]) : List[A] = {
    foldLeft(bigList, Nil : List[A])(append)
  }

  def append[A](head : List[A], tail : List[A]) : List[A] = {
    List.leftFoldRight(head, tail)((a,b) => Cons(a,b))
  }
}


val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this one matches
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

//3.8 what happens? nothing apparently.
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
List.leftFoldRight(List(1,2,3,4), 0)(_+_)
List.length(List(1,2,3,4))
List.leftSum(List(1,4,5))
List.leftProduct(List(4,5))
List.leftLength(List(1,2,3,4,5,6))
List.reverse(List(1,2,3))
List.append(List(1,2), List(3,4))
List.flatten(List(List(1,2), List(3,4), Nil))

