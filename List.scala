sealed trait ListMine[+A]
case object Nil extends ListMine[Nothing]
case class Cons[+A](head: A, tail: ListMine[A]) extends ListMine[A]


object ListMine {
  def tail[A](ints : ListMine[A]):ListMine[A] = ints match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def setHead[A](head:A,ints : ListMine[A]):ListMine[A] = ints match {
    case Nil => Nil
    case Cons(_,xs) => Cons(head,xs)
  }

  def init[A](l: ListMine[A]): ListMine[A] =
    l match {
      case Nil => sys.error("init of empty ListMine")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def initMine[A](l: ListMine[A]): ListMine[A]={
    def go(l: ListMine[A]): ListMine[A] = l match {
      case Nil => l
      case Cons(x:A,Cons(xs,Nil)) => Cons(x,Nil)
      case Cons(x:A,xs: ListMine[A]) => Cons(x,go(xs))
    }
    go(l)
  }

  //Accumulate results and then reverse
  def init2[A](l: ListMine[A]): ListMine[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: ListMine[A]): ListMine[A] = cur match {
      case Nil => sys.error("init of empty ListMine")
      case Cons(_,Nil) => ListMine(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def drop[A](l: ListMine[A], n: Int): ListMine[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropMine[A](l: ListMine[A], n: Int): ListMine[A] = {
    @annotation.tailrec
    def go(n: Int, l: ListMine[A]): ListMine[A] = {
      if (n <= 1) ListMine.tail(l)
      else go(n - 1, ListMine.tail(l))
    }
    go(n, l)
  }

  def dropWhile[A](l: ListMine[A], f: A => Boolean): ListMine[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def dropWhileMine[A](l: ListMine[A], f: A => Boolean): ListMine[A]={
    def go(l: ListMine[A]): ListMine[A] = l match {
      case Nil => l
      case Cons(x:A,xs: ListMine[A]) if f(x) => go(xs)
      case Cons(x:A,xs: ListMine[A]) => Cons(x,go(xs))
    }
    go(l)
  }

  def sum(ints: ListMine[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: ListMine[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): ListMine[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Listmanipulation
{
    // The next line is the value that I want to return.
    val x = ListMine(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + ListMine.sum(t)
      case _ => 101
    }

    ListMine(1,2,3) match { case Cons(_,t) => t }
    ListMine(1,2,3) match { case Cons(h,_) => h }
    ListMine.sum(ListMine(1,2,3))
    ListMine.tail(ListMine(1,2,3))
    ListMine.setHead(5,ListMine(1,2,3))
    ListMine.drop(ListMine(1,2,3),1)
    ListMine.drop(ListMine(1,2,3),2)
    ListMine.dropWhile(ListMine(1,2,3),(x:Int) => x == 3)
    ListMine.init(ListMine(1,2,3,4))
}