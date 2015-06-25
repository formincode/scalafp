object SimpleRNG {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def randomPair(rng: RNG): ((Int, Int),RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2),rng3)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) ={
    val (i1, rng2) = rng.nextInt
    (i1, rng2) match {
      case (n,rng) if (n==Int.MinValue) => (0,rng)
      case (n,rng) if (n<0) => (n * -1,rng)
      case (n,rng) => (n,rng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    ((i/Int.MaxValue.toDouble),rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d,rng3) = double(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)

    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeInt(rng: RNG): (Int, RNG) ={
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleRand: Rand[Double] = {
    map(nonNegativeInt)(i => i/Int.MaxValue.toDouble)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
        both(_.nextInt, double)

  val randDoubleInt: Rand[(Double, Int)] =
        both(double, _.nextInt)

  //def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = _
    //fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
}