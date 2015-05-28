
object Mymodule {

  def abs(n:Int): Int = 
    if (n<0) -n
    else n

  def factorial(n:Int)={
    @annotation.tailrec
    def go (n:Int, acc:Int):Int=
       if (n<=0) acc
       else go(n-1,n*acc)

    go(n,1)
  }

  private def formatResult(str:String,x:Int,f:Int=>Int)={
    val msg = "The %s of %d is %d"
    msg.format(str,x,f(x))
  }

  private def formatAbs(x:Int) = {
    val msg = "The abs of %d is %d"
    msg.format(x,abs(x))
  }

  private def formatFactorial(x:Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(x,factorial(x))
  }

  def fib(n:Int):Int= {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n <= 1) prev
      else go(n-1, curr, prev + curr)
    }
    go(n,0,1)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
