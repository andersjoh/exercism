//import scala.collection.mutable.HashMap
import collection.mutable

object nameFactory {
  val emptyMap = mutable.HashSet.empty[String];
  val letters: List[Char] = ('A' to 'Z').toList
  val numbers: List[Int] = (0 to 9).toList
  val rng = new scala.util.Random(23234)
  val nameGen: (Int, Int) => String = (numLetters, numNumbers) => {
    // println(emptyMap.size)
    val name = recursiveGenerator(numLetters, numNumbers)
    emptyMap += name
    println(s"$name added to hashSet")
    name
  }
  val getRandom = (l: List[Any]) => {

    l(rng.nextInt(l.size))
  }
  @scala.annotation.tailrec
  val recursiveGenerator: (Int, Int) => String =
    (numLetters, numNumbers) => {
    //   if (maxAttempts == 0) {
    //     throw new Exception("MaxAttempts reached!")
    //   }
      var let = (0 until numLetters)
        .map(_ => getRandom(letters))
      var numb = (0 until numNumbers).map(_ => getRandom(numbers))
      var name = (let ++ numb).mkString

      var out = name match {
        case candidate if !emptyMap.contains(candidate) => candidate
        case candidate => {
          println(s"$candidate exists - recursing")
          recursiveGenerator(numLetters, numNumbers)
        }
      }
      println(s"found candidate $out")
      out
    }
  def removeKey(name: String) = {
    emptyMap -= name
  }

}

class Robot {
  private var _name = ""
  def name = {
    if (_name.isBlank)
      _name = nameFactory.nameGen(2, 3)
    _name
  }
  def reset() = {
    nameFactory.removeKey(_name)
    _name = ""
  }

}
