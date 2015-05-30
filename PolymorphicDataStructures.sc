sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

val ex1: List[Double] = Nil
val ex2:List[Int] = Cons(1,Nil)
val ex3: List[String] = Cons("A", Cons("b",Nil))

val ex4 = Cons(4,Nil)
ex4.toString
ex3.toString



