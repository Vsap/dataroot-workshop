package test2
// #1
object Test {

  // your code goes here
  //val w = "GHMABGZ VKXTMXL LNVVXLL EBDX GHG-LMHI, XGMANLBTLMBV XYYHKM"
  val word: String = "GHMABGZVKXTMXLLNVVXLLEBDXGHGLMHIXGMANLBTLMBVXYYHKM"
  val abc = List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  val dic: Map[Char, Int] = (abc zip (0 to 25).toList).toMap
  val drev = for ((a, b) <- dic) yield (b, a)

  def encrypt(k: Int, pt: String): String = {
    for (l <- pt) yield drev((dic(l) + k) % 26)
  }

  def decrypt(ct: String): List[String] = {
    (for (i <- 1 to 25) yield encrypt(i, ct)).toList
  }

  val check = decrypt(word)
  val Key: Int = 7
  val PlainWordIs = "NOTHINGCREATESSUCCESSLIKENONSTOPENTHUSIASTICEFFORT"
  println(s"#1 - ${"ncslnee"}")

  // #2

  // your code goes here
  val a1 = "10001011101010101010000111110111011110101010101101110101010101010010000010110100101010101011011010100101011010101010101010101010101110101011000101101011110101010101010101010001010101010101101010101010101010101010101010111000001010101111010100111010101001011101010111111111101010101111111101010111110101001010101111110111101011010111111101011110101111111111111101111111111010101111101010101001111101010101010100100101010111101001010101001010101001010111110101010101010101011110101010010101001111101010100101111101010101001111111111101010111111111101001010111111110110101001111101010101111111010110100011111111111010101101011111110101010101110101010101010001110111101010101010101010101000001010110111111010101010010101011110101010000001010101000000000000101001111100000000000010010101010000001"
  val a2 = "11100101000010101000001010010000010101011000110000110101000001010100000010000000010101100000110100100010111111111111111010010001010000001000000100000101011110101000000001010100000001010100101010111001010100000000000010101010101101010010101010101111001010000000000000001010010100111000010000000010100001010101000000110000001010101000000000000101001111100000000000010010101010000001"
  val b1 = a1.toList.reverse map(_.toString.toInt)
  val b2 = a2.toList.reverse map(_.toString.toInt)
  val (b1short,b1rest)= (b1 take b2.length, b1 drop b2.length)
  def add(b1: List[Int], b2: List[Int]):(Int,List[Int]) = {
    def addf(answ:List[Int],carry: Int, a1: List[Int], a2: List[Int]): (Int,List[Int]) = if (a1.isEmpty||a2.isEmpty) (carry,answ) else (a1.head, a2.head) match {
      case (1, 0) => if(carry == 0){addf(1::answ,0, a1.tail, a2.tail)} else{addf(0::answ,1, a1.tail, a2.tail)}
      case (0, 1) => if(carry == 0){addf(1::answ,0, a1.tail, a2.tail)} else{addf(0::answ,1, a1.tail, a2.tail)}
      case (1, 1) => if(carry == 0){addf(0::answ,1, a1.tail, a2.tail)} else{addf(1::answ,1, a1.tail, a2.tail)}
      case (0, 0) => if(carry == 0){addf(0::answ,0, a1.tail, a2.tail)} else{addf(1::answ,0, a1.tail, a2.tail)}
      case (_,_) => (carry,answ)
    }
    addf(List(),0, b1, b2)
  }
  def dif(list:List[Int]):Int ={
    list.length-2*list.sum
  }
  val Answer = dif((add(b1short, b2)._2) ++ b1rest) //73
  println(s"#2 - ${73}")

  // #3
  def palSum(current:Int,stay:Int, sum: Int):Int = isPal(current.toBinaryString.toList) match{
    case true => if(stay > 0) palSum(current+1,stay-1,sum+current) else sum
    case false => if(stay > 0) palSum(current+1,stay,sum) else sum
    case _ => sum
  }
  def isPal[A](l: List[A]):Boolean = l match {
    case List() => true
    case List(_) => true
    case h::t => (h == t.last && isPal(t.init))
  }
  println(palSum(1,73,0))
  println(s"#3 - ${34223}")

  // #4

  val set = List(-1, -1, -2, -2, 1, -5, 1, 0, 1, 14, -8, 4, 5, -11, 13, 5, 7, -10, -4, 3, -6, 8, 6, 2, -9, -1, -4, 0)
  def tripleGen(set:List[Int]): Set[(Int,Int,Int)] ={
    ((for{
      i <- set.filter(_ > 0).distinct
      j<- set.filter(_ < 0).distinct
      k <- (set.filter(_ < 0).takeWhile(_ != j) ++
        set.filter(_ < 0).dropWhile(_ != j)).filter(_ < j)
      if i+j+k == 0
    } yield (i,j,k)) ++
      (for{
        i <- set.filter(_ < 0).distinct
        j<- set.filter(_ > 0).distinct
        k <- (set.filter(_ > 0).takeWhile(_ != j) ++
          set.filter(_ > 0).dropWhile(_ != j)).filter(_ >= j)
        if i+j+k == 0
      } yield (i,j,k)) ++
      (for{
        i <- set.filter(_ > 0).distinct
        j <- set.filter(_ < 0).distinct
        if i+j==0
      } yield (i,0,j))).toSet
  }
  println(tripleGen(set).size)

  println(s"#4 - ${42}")

  // #5

  import scala.io.Source
  val source = Source.fromFile("task5.txt")
  val line = source.getLines.toList.map(_.toDouble)
  println(line.sum.toString.toList.take(10))

  println(s"#5 - ${5537376230}")
}