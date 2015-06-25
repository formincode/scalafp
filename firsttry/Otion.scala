object Option {
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match
    {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    {
      case None => ob
      case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match
    {
      case Some(a) if f(a) => this
      case _ => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)


def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match
{
  case (None,_) => None
  case (_,None) => None
  case (a,b) => Some(f(a.get,b.get))
}

def map2For[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)


def sequence[A](a: List[Option[A]]): Option[List[A]] =
{
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]
  for (x <- a)
    x match {
      case None => None
      case x => buf.append(x.get)
    }
  Some(buf.toList)
}
