import java.util.Scanner

object Challenge05DNASplicer2 {

  def main (args: Array[String]): Unit = {
    if (args.length > 0) {
      println(args(0)) // Starting TEST or SUBMIT
    }
    val scanner = new Scanner(System.in)
    while (scanner.hasNextLine) {
      val line = scanner.nextLine()
      Console.err.println(line)
      if (!line.startsWith(">")) {
        val pieces = line.split(" ")
        val length = pieces.length

        def s (l: List[Int]) = l.map(pieces(_)).mkString

        def range = (0 until length).toSet

        val r = (1 until length).flatMap(range.subsets(_)).flatMap(s => {
          range.filter(!s.contains(_)).subsets.filter(_.nonEmpty).map(s2 => (s.toList, s2.toList))
        }).flatMap(combo => {
          combo._1.permutations.flatMap(s1 => {
            combo._2.permutations.map(s2 => {
              (s1, s2)
            })
          })
        }).find(candidate => {
          s(candidate._1) == s(candidate._2)
        }).flatMap(solution => {
          Option((solution._1 ++ solution._2).sorted.map(_ + 1).mkString(","))
        })

        r match {
          case Some(x) => {
            Console.err.println(x)
            println(x)
          }
          case _ => println("No encontrado")
        }

      }
    }
  }
}
