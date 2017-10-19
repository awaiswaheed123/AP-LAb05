object Lab05 extends App {
  def FindPrime(n:Int) = {
    //finding square root for the loop to run
     def sqrt(num: Double) = {
        def abs(x: Double) = if(x < 0) -x else x
        def isGoodEnough(est: Double):Boolean = ( abs(est * est - num)/num < 0.00000000000001 )
        def guess(est: Double): Double = if(isGoodEnough(est)) est else guess(improve(est))
        def improve(est: Double): Double = ( (num / est) + est )/2
        guess(1)
    } 
    val sq = sqrt(n)
    //getting the list of all numbers
    val list = List.range(2,n)
    def loop(x:Int,sublist:List[Int]): List[Int]={
      //Base condition
      if(x>sq) sublist
      else
        loop( x+1 , sublist.filterNot( (y) => (y%x==0 && y!= x) ) )//Filtering the leftouts i.e. the non-multiples
    }
    loop(2, list)
  }
  
  def TestFindPrime(): Boolean = {
    val list = FindPrime(10)
    val list2 = List(2,3,5,7)
    if(list == list2) true
    else false
  }
  
  if(TestFindPrime()) {
    println("Test Succesful.")   
    println("Please Enter a number: ")
    val x = scala.io.StdIn.readInt()
    println("Prime numbers until "+ x + " are: ")
    FindPrime(x).foreach(println)
  }
  else 
    println("Test un-succesful")

}