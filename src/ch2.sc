//2.1 tail recursive fibonnaci generator
def fib(n : Int) : Int = {
  @annotation.tailrec
  def iter(a: Int, b: Int, m : Int): Int = {
    if(m >= n) a + b
    else iter(b, a + b, m + 1)
  }
  iter(0, 1, 0)
}

//2.2 sorted array checking
@annotation.tailrec
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean =
  as.length match {
    case n if n <= 1 => true
    case _ => {
      if (!ordered(as(0), as(1))) false
      else isSorted(as.tail, ordered)
    }
  }

//2.3 currying
def curry[A, B, C](f : (A,B) => C) : A => (B => C) = {
  a : A => b : B => f(a,b)
}

//2.4 uncurrying
def uncurry[A,B,C](f : A => B => C) : (A,B) => C = {
  (a,b) => f(a)(b)
}

//2.5 composition
def compose[A,B,C](f : B => C, g : A => B) : A => C = {
  a => f(g(a)) // back to 9th grade!
}