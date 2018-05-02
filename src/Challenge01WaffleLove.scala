import java.util.Scanner

object Challenge01WaffleLove {

  def solve (width: Int, height: Int): Int = (width - 1) * (height - 1)

  def main (args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextInt()) {
      scanner.nextLine()
      println(s"Case #$i: ${solve(scanner.nextInt() /* width */, scanner.nextInt() /* height */)}")
    }
  }
}
