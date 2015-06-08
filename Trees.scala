/**
 * Created by manish.srivastava on 6/8/15.
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](as:Tree[A]):Int = as match
  {
    case Leaf(_) => 1
    case Branch(l,r) => size(l)+size(r)+1
    case _ => 0
  }

  def maximum(as:Tree[Int]):Int = {
    as match {
      case Leaf(value) => value
      case Branch(l,r) => maximum(l).max(maximum(r))
    }
  }

  def depth[A](as:Tree[A]):Int = as match{
    //case Branch(l,r) => size(l).max(size(r))
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](as:Tree[A])(f:A=>B):Tree[B] = as match{
    case Leaf(n) => Leaf(f(n))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldSum[A,B](t: Tree[A])(f: A => B): Int =
    fold(t)(a => 1)(_+_+1)

  def foldMax[A,B](t: Tree[Int])(f: A => B): Int =
    fold(t)(a => a)(_ max _)

  def foldDepth[A,B](t: Tree[A])(f: A => B): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def foldMap[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(n => Leaf(f(n)):Tree[B])(Branch(_,_))


}