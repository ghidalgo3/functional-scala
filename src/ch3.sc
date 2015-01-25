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

  def increment(list : List[Int]) : List[Int] = {
    foldRight(list, Nil : List[Int])((a, b) => Cons(a + 1, b))
  }

  def double2String(list : List[Double]) : List[String] = {
    foldLeft(list, Nil: List[String])((a , b) => Cons(b.toString, a))
  }

  def flatten[A](bigList : List[List[A]]) : List[A] = {
    foldLeft(bigList, Nil : List[A])(append)
  }

  def append[A](head : List[A], tail : List[A]) : List[A] = {
    List.leftFoldRight(head, tail)((a,b) => Cons(a,b))
  }

  def filter[A](as : List[A])(f : A => Boolean) : List[A] = {
    foldRight(as, Nil : List[A]){ (elem, accum) =>
      if(f(elem)) Cons(elem, accum) else accum
    }
  }

  def flatMap[A,B](as : List[A])(f : A => List[B]) : List[B] = {
    flatten(map(as)(f))
  }

  def sumLists(as : List[Int], bs:List[Int]) : List[Int] = {
    (as, bs) match {
      case (Nil, _) => bs
      case (_ , Nil) => as
      case (Cons(a,b),Cons(h,t)) => Cons(a+h, sumLists(b,t))
    }
  }

//  def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = {
//    foldLeft(sup, sub)((b,a) => if())//b is the sublist, a is an element of super list
//  }

  def zipWith[A](as : List[A], bs:List[A])(f : (A,A) => A) : List[A] = {
    (as, bs) match {
      case (Nil, _) => bs
      case (_ , Nil) => as
      case (Cons(a,b), Cons(h,t)) => Cons(f(a,h), zipWith(b,t)(f))
    }
  }

  def filterWithFlatMap[A](as : List[A])(filter : A => Boolean) : List[A] = {
    flatMap(as){a => if(filter(a)) Cons(a, Nil) else Nil}
  }

  def map[A,B](list : List[A])(f : A => B) : List[B] = {
    foldRight(list, Nil: List[B])((a,b) => Cons(f(a), b))
  }
}


val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this one matches
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
sealed trait Tree[+A]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left : Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  def size[A](t : Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }
  }

  def maximum(t : Tree[Int]) : Int= {
    t match {
      case Leaf(a) => a
      case Branch(l,r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t : Tree[A]) : Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => depth(l) max depth(r)
    }
  }

  def map[A,B](t : Tree[A])(f : A => B) : Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

//  def fold[A,B](t : Tree[A], z : B)(f : (A,B) => B) : B = {
////    t match {
////      case Leaf(a) => f(a,z)
////      case Branch(l,r) => f(l , f(r, z))
////    }
//  }
}


val nums = List(1,2,3,4,5)
val arr = Array[Int](1,2,3,4)
1 + 2 == 1.+(2)

val sum : (Int, Int) => Int  = _ + _

val exampleTree =
  Branch(
    Branch(
      Leaf(1), Branch(
        Leaf(4), Leaf(10)
      )),
    Leaf(3))
Tree.size(exampleTree)
Tree.maximum(exampleTree)
Tree.map(exampleTree)(_ / 2)