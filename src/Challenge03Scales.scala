import java.util.Scanner

object Challenge03Scales {

  final val allNotes = Array("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
  final val W = 2
  final val H = 1
  final val M = ('M', List(W, W, H, W, W, W, H))
  final val m = ('m', List(W, H, W, W, H, W, W))

  case class Scale (name: String, notes: List[Int])

  val allScales = {
    for (
      t <- List(M, m);
      i <- 0 until allNotes.length
    ) yield {
      Scale( s"${t._1}${allNotes(i)}", t._2.scan(0)(_ + _).drop(1).map(note => (i + note) % allNotes.length).sorted)
    }
  }

  def note (f: String) : String = {
    val delta =
      if (f.endsWith("#"))
        1
      else if (f.endsWith("b"))
        -1
      else
        0
    allNotes((allNotes.indexOf(f.substring(0, 1)) + allNotes.length + delta) % allNotes.length)
  }

  def printSolution (c: Int, scales: List[Scale]) = {
    val s = if (scales.isEmpty) List(Scale("None", List())) else scales
    println(s"Case #$c: ${s.map(_.name).mkString(" ")}")
  }

  def main (args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextLine().toInt) {
      // Skip 0 notes tunes
      if (scanner.nextLine().toInt > 0) {
        // Convert tune to unique notes
        val tune = scanner.nextLine()
        val uniqs = tune.split(" ").map(note).map(allNotes.indexOf(_)).toSet

        // Filter scales and print result
        val r = allScales.filter(s => uniqs.forall(s.notes.contains(_)))
        printSolution(i, r)
      } else {
        printSolution(i, allScales)
      }
    }
  }
}
