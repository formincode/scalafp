object FindFirst {

  /*def findFirst(ss:Array[String],key:String):Int={
    def loop(n:Int):Int=
      if (n>=ss.length) -1
      else if (ss(n)==key) n
      else loop(n+1)
    loop(0)
  }*/

  def findFirstGen[A](ss:Array[A],f: A=> Boolean):Int={
    @annotation.tailrec
    def loop(n:Int):Int=
      if (n>=ss.length) -1
      else if (f(ss(n))) n
      else loop(n+1)
    loop(0)
  }

  //def finder[A](s:A):Boolean=s=="c"
  //FindFirst.findFirstGen(Array("a","b","c"),finder)
  //FindFirst.findFirstGen(Array("a","b","c"),(x:String) => x == "c")

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean={
    @annotation.tailrec
    def loop(n:Int):Boolean=
      if (n>=as.length) true
      else if (!ordered(as(n),as(n+1))) false
      else loop(n+1)
    loop(0)
  }
  //isSorted(Array(1,2), (x:Int,y:Int) => x < y)
}

