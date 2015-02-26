trait RNG {
  def nextInt : (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG{
  def nextInt: (Int, RNG) = {
    val newSeed =  (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

}

//6.1
def nonNegativeInt(rng : RNG) : (Int,RNG) = {
  val (num, r1) = rng.nextInt
  num match {
    case positive if num > 0 => (num, r1)
    case maxNegative if num == Int.MinValue => (-1 * (maxNegative + 1), r1)
    case _ => (num * -1, r1)
  }
}

//6.2
def double(rng: RNG) : (Double, RNG) = {
  val (num, r1) = rng.nextInt
  (Math.abs(num + 1).toDouble / Int.MaxValue, r1)
}

//6.3
def intDouble(rng: RNG) : ((Int, Double), RNG) = {
  val (a, r1) = rng.nextInt
  val (b, r2) = double(r1)
  ((a, b), r2)
}
def doubleInt(rng:RNG) : ((Double, Int), RNG) = {
  val (a, r1) = rng.nextInt
  val (b, r2) = double(r1)
  ((b, a), r2)
}

def double3(rng: RNG) : ((Double, Double, Double) , RNG) = {
  val (a, rand1) = double(rng)
  val (b, rand2) = double(rand1)
  val (c, rand3) = double(rand2)
  ((a,b,c), rand3)
}

//6.4
def ints(count: Int)(rng : RNG) : (List[Int], RNG) = {
  var rand = rng
  var list : List[Int] = List()
  for(i <- 0 until count) {
    val state = rand.nextInt
    rand = state._2
    list = list :+ state._1
  }
  (list, rand)
}

type Rand[+A] = RNG => (A, RNG)

val int : Rand[Int] = _.nextInt // this is a function RNG => (Int, RNG)

def unit[A](a : A) : Rand[A] = {
  rng => (a, rng)
}

//given the state transition function of A, and a A => B
//returns the state transition function of B
def map[A,B](s : Rand[A])(f : A => B) : Rand[B] = {
  rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }
}

//this function doesn't actually give you the answer, just the function to
//compute the answer
def nonNegativeEven: Rand[Int] = {
  map(nonNegativeInt)(i => i - i % 2)
}

//6.5
def doubleMap : Rand[Double] = {
  map[Int, Double](nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
}

//6.6
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
  rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }
}

def both[A,B](ra: Rand[A], rb: Rand[B]) : Rand[(A,B)] = map2(ra, rb)((_,_))
val randIntDouble_ : Rand[(Int, Double)] = both(int, double)
val randDoubleInt_ : Rand[(Double, Int)] = both(double, int)
////6.7
//def sequence[A](fs : List[Rand[A]]) : Rand[List[A]] = {
//  rng => {
//    fs.fold(unit(_))((a,b) )
//  }
//}

//6.8
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
  rng : RNG => {
    val (a, r1) = f(rng)
    val rb : Rand[B] = g(a)
    return rb //wtf scala
  }
}

def map_[A,B](s : Rand[A])(f : A => B) : Rand[B] = {
  flatMap(s)(a => rng => (f(a), rng))
}

def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f : (A,B) => C): Rand[C] = {
  flatMap(ra)(a => map(rb)(b => f(a, b)))
}