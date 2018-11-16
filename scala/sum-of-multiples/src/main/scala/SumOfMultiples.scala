object SumOfMultiples {
  val isMultiple : (List[Int], Int) => Boolean = (factors, n)  => factors match {
        case Nil => false
        case head :: tail => (n % head == 0) || isMultiple(tail, n) 
    }
  
  def sum(factors: Set[Int], limit: Int): Int = { 
    val r = 0 until limit 
    
    val multiples = for { i <- r if isMultiple(factors.toList, i) } yield i

    multiples.sum
  }

}

