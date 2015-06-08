sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


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
      case Nil => sys.error("init of empty List")
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
      case Nil => sys.error("init of empty List")
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
      case Cons(h,t) if (f(h)) => dropWhile(t, f)
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

  def foldRight[A,B](as:List[A],z:B)(f:(A,B) => B):B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs,z)(f))
  }

  /*
  List.foldLeft(List(1,2,3),0)(_ + _)
  foldLeft(List(2,3),f(1,0))
  foldLeft(List(3),f(2,f(1,0)))
  foldLeft(List(),f(3,f(2,f(1,0))))
  */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(h,z))(f)
  }

  def productRight(as: List[Int])=
    foldRight(as,1.0)(_ * _)

  def sumRight(as: List[Int])=
    foldRight(as,0)(_ + _)

  def lengthRight[A](as: List[A]) =
    foldRight(as,0)((_,acc) => acc + 1)

  def productLeft(as: List[Int])=
    foldLeft(as,1.0)(_ * _)

  def sumLeft(as: List[Int])=
    foldLeft(as,0)(_ + _)

  def lengthLeft[A](as: List[A]):Int =
    foldLeft(as,0)((_,acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((x,y)=>Cons(x,y))

  def append[A](l: List[A],a:List[A]):List[A] =
    foldRight(l, a)((x,y)=>Cons(x,y))

  /*
  foldRight(List(List(1),List(2),List(3)), Nil)(append)
  append(List(1),foldRight(List(List(2),List(3)), Nil)(append))
  append(List(1),append(List(2),foldRight(List(List(3)), Nil)(append))
  append(List(1),append(List(2),append(List(3),foldRight(List(List())), Nil)(append))
  append(List(1),append(List(2),append(List(3), Nil)
   */
  def appendList[A](l: List[List[A]]):List[A] =
    foldRight(l, Nil:List[A])((a,b)=>append(a,b))

  def addOne(l:List[Int]):List[Int]=
    foldRight(l,Nil:List[Int])((h,t)=>Cons(h+1,t))

  def doubleToString(l:List[Double]):List[String]=
    foldRight(l,Nil:List[String])((h,t)=>Cons(h.toString,t))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as,Nil:List[B])((h,t)=>Cons(f(h),t))

  def map_2[A,B](as: List[A])(f: A => B): List[B] =
    reverse(foldLeft(as,Nil:List[B])((h,t)=>Cons(f(h),t)))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as,Nil:List[A])((h,t)=>if (f(h)) Cons(h,t) else t)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as,Nil:List[B])((h,t) => append(f(h),t))
    //concat(map(as)(f))

  def flatMapFilter[A](as: List[A])(f: A => Boolean):List[A]=
    flatMap(as)(h => if (f(h)) List(h) else Nil)

  def zipWith[A](as:List[A],bs:List[A])(f: (A,A)=> A):List[A]= (as,bs) match {
      case (_,Nil) => Nil
      case (Nil,_) => Nil
      case (Cons(h:A,t),Cons(h2:A,t2)) => Cons(f(h,h2),zipWith(t,t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (Nil,Nil) => true
    case (Nil,_) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Listmanipulation
{
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
    //Returns list
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    //Reverses above
    List.foldLeft(List(1,2,3), Nil:List[Int])((x,y)=>Cons(x,y))
    //Append
    List.foldRight(List(1,2,3), List(4))((x,y)=>Cons(x,y))
}