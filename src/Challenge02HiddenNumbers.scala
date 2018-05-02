import java.util.Scanner

object Challenge02HiddenNumbers {

  def solve (number: Int, length: Int): BigInt = {
    if (length == 2) {
      2 - 2 // 10 - 10
    } else {
      val l1 = length - 1
      val max = (l1 to 0 by -1).toList
      val min = 1 :: 0 :: (2 to l1).toList
      (max zip min foldLeft BigInt(0)) (
        (acc: BigInt, current: (Int, Int)) =>
          acc + (BigInt(length).pow(current._1) * (current._1 - current._2))
      )
    }
  }

  def main (args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextLine().toInt) {
      val r = solve(i, scanner.nextLine().length)
      println(s"Case #$i: $r")
    }
  }
}
