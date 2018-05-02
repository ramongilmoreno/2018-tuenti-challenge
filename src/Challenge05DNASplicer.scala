import java.util.Scanner

object Challenge05DNASplicer {

  def rear (input: Array[String], values: List[Int]) = {
    val r = values.filter(_ != Int.MaxValue).flatMap(input(_)).mkString
    // println("Rear: " + r)
    r
  }
  def front (input: Array[String], values: List[Int]) = {
    // print("Front: ")
    rear(input, values.reverse)
  }

  def solve (input: Array[String], index: Boolean, tried: Boolean, candidates: Array[List[Int]]): Option[Set[Int]] = {
    // Identify used values
    val used = candidates.flatMap(_.toSet).toSet

    // Check current solution
    val s1 = front(input, candidates(0))
    val s2 = front(input, candidates(1))
    val r1 = rear(input, candidates(2))
    val r2 = rear(input, candidates(3))

    if (used.nonEmpty && (((s1 + r1) == (s2 + r2)) || ((s1 + r2) == (s2 + r1)))) {
      // Solution found
      Some(used)
    } else {
      // Try to locate a pair that fits
      val unused = (0 until input.length).toSet.diff(used)
      if (unused.isEmpty)
        // No solution here
        None
      else {
        def filter (step: List[Int]): Boolean = {
          if (index) {
            val s1 = front(input, step.head :: candidates(0))
            val s2 = front(input, step.tail.head :: candidates(1))
            s1.startsWith(s2) || s2.startsWith(s1)
          } else {
            val e1 = rear(input, step.head :: candidates(2))
            val e2 = rear(input, step.tail.head :: candidates(3))
            e1.endsWith(e2) || e2.endsWith(e1)
          }
        }
        val two = unused.subsets(2).toList.map(_.toList).flatMap(_.permutations).filter(filter)
        if (two.isEmpty && !tried) {
          // Try the other directrion with the same candidates
          solve(input, !index, true, candidates)
        } else {
          val one = unused.toList.flatMap(c => {
            List(List(c, Int.MaxValue), List(Int.MaxValue, c))
          }).view.filter(filter)
          (two ++ one).view.flatMap(step => {
            if (index)
              solve(input, !index, false, Array(step.head :: candidates(0), step.tail.head :: candidates(1), candidates(2), candidates(3)))
            else
              solve(input, !index, false, Array(candidates(0), candidates(1), step.head :: candidates(2), step.tail.head :: candidates(3)))
          }).find(_ != None)
        }
      }
    }
  }

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

        // Try to find 4 pieces that match start and end
        ((0 until pieces.length).map(List(_)).toSet.subsets(4).flatMap(_.toList.permutations).map(_.toArray).filter(c => {
          val s1 = front(pieces, c(0))
          val s2 = front(pieces, c(1))
          val e1 = rear(pieces, c(2))
          val e2 = rear(pieces, c(3))
          (s1.startsWith(s2) || s2.startsWith(s1)) && (e1.endsWith(e2) || e2.endsWith(e1))
        }).toList ++ List(Array.fill(4)(List[Int]()))).view.flatMap(edges => {
          solve(pieces, true, false, edges)
        }).find(_ != None) match {
          case Some(x) => {
            val r = x.filter(_ != Int.MaxValue).map(_ + 1).toList.sorted.mkString(",")
            Console.err.println(r)
            println(r)
          }
          case _ => println("Not found!")
        }
      }
    }
  }
}
