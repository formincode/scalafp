// These 3 lines define a simplified List type
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// This companion object contains basic operations for the List type
object List {
  def tail[A](ints : List[A]):List[A] = ints match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def setHead[A](head:A,ints : List[A]):List[A] = ints match {
    case Nil => Nil
    case Cons(_,xs) => Cons(head,xs)
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
  }

  def initMine[A](l: List[A]): List[A]={
    def go(l: List[A]): List[A] = l match {
      case Nil => l
      case Cons(x:A,Cons(xs,Nil)) => Cons(x,Nil)
      case Cons(x:A,xs: List[A]) => Cons(x,go(xs))
    }
    go(l)
  }

  //Accumulate results and then reverse
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropMine[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(n: Int, l: List[A]): List[A] = {
      if (n <= 1) List.tail(l)
      else go(n - 1, List.tail(l))
    }
    go(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def dropWhileMine[A](l: List[A], f: A => Boolean): List[A]={
    def go(l: List[A]): List[A] = l match {
      case Nil => l
      case Cons(x:A,xs: List[A]) if f(x) => go(xs)
      case Cons(x:A,xs: List[A]) => Cons(x,go(xs))
    }
    go(l)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

// The next line is the value that I want to return.
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

List(1,2,3) match { case Cons(_,t) => t }
List(1,2,3) match { case Cons(h,_) => h }
List.sum(List(1,2,3))
List.tail(List(1,2,3))
List.setHead(5,List(1,2,3))
List.drop(List(1,2,3),1)
List.drop(List(1,2,3),2)
List.dropWhile(List(1,2,3),(x:Int) => x == 3)
List.init(List(1,2,3,4))





