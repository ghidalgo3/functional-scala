//option

case class Some[A](value : A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
  def map[B](f : A => B) : Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case _ => None
    }
  }

  def flatMap[B](f : A => Option[B]) : Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default : => B) : B = {
    this match {
      case Some(a) => a
      case _ => default
    }
  }

  def orElse[B >: A](ob : => Option[B]) : Option[B] = {
    map(a => Some(a)).getOrElse(ob)
  }

  def filter(f : A => Boolean) : Option[A] = {
    flatMap(a => if(f(a)) Some(a) else None)
  }
}

object Option {
}

def average(l : Seq[Double]) : Option[Double] = {
  if(l.length > 0) Some(l.sum / l.length) else None
}

def variance(xs : Seq[Double]) : Option[Double] = {
  average(xs) flatMap (m => average(xs.map(x => math.pow(x - m, 2))))
}

variance(List(1,2,3,5))


