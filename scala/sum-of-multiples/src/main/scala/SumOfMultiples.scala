object SumOfMultiples {
  val isMultiple = List[Int], Int => (factors: List[Int], n: Int) : Boolean => { 
      factors match {
        case Nil => false
        case head :: tail => (n % head == 0) || isMultiple(tail, n) 
    }
  }
  def sum(factors: Set[Int], limit: Int): Int = { 
    val r = (0 until limit)  
    
    val multiples = r match {
      case i if isMultiple(factors, i) => i
    }  
    sum(multiples)
  }

}

