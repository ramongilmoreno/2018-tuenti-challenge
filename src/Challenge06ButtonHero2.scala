import java.util.Scanner

import scala.collection.mutable

object Challenge06ButtonHero2 {

  case class Note (id: Int, start: Int, length: Int, speed: Int, points: Int)

  case class Event (note: Note, start: Boolean, time: Float) extends Ordered[Event] {
    override def compare(that: Event): Int = {
      val startDiff = this.time - that.time
      if (startDiff != 0)
        startDiff.toInt
      else if (this.start == that.start)
        this.note.id - that.note.id
      else if (this.start)
        -1
      else
        1
    }
  }

  trait Status { def down: Float; def score: Int }
  case class Down (down: Float, score: Int, pending: Set[Note]) extends Status
  case class Up (down: Float, up: Float, score: Int) extends Status

  def start (note: Note): Float = note.start.toFloat / note.speed
  def end (note: Note): Float = (note.start + note.length).toFloat / note.speed

  def solve(downs: Set[Down], ups: Set[Up], events: mutable.SortedSet[Event]): Int = {
    if (events.isEmpty) {
      (downs ++ ups).maxBy(_.score).score
    } else {
      val event = events.head
      val note = event.note
      val time = event.time
      var nextDowns: Set[Down] = downs
      var nextUps: Set[Up] = ups

      if (event.start) {
        nextDowns = downs.map(_ match {
          case d if (d.down == time) => Down(d.down, d.score, d.pending + note)
          case d => d
        })
        nextDowns = nextDowns ++ ups.filter(_.up < time).map(u => Down(time, u.score, Set(note)))
      } else {
        // Compute downs
        val (candidateDowns, otherDowns) = downs.partition(_ match {
          case d if (d.down == start(note)) => true
          case _ => false
        })
        nextDowns = candidateDowns.map(d => Down(d.down, d.score, d.pending - note)) ++ otherDowns

        // Sum scores for ups
        val (candidateUps, otherUps) = ups.partition(_ match {
          case u if (u.down == start(note) && u.up == end(note)) => true
          case _ => false
        })
        nextUps = candidateDowns.map(d => Up(d.down, time, d.score + note.points)) ++
          candidateUps.map(u => Up(u.down, time, u.score + note.points)) ++
          otherUps
      }

      // Filter ups: retain current ups and max ups
      val (nowUps, oldUps) = nextUps.partition(_.up == time)
      if (oldUps.isEmpty)
        nextUps = nowUps
      else
        nextUps = nowUps + oldUps.maxBy(_.score)

      // Clean empty downs
      nextDowns = nextDowns.filter(_.pending.nonEmpty)

      // Discard unpromising button down
      val max = nowUps.map(_.score).foldLeft(Int.MinValue)(Math.max(_, _))
      nextDowns = nextDowns.filter(d => {
        d.down == time || d.pending.groupBy(end(_)).map(_._2.map(_.points).sum).foldLeft(d.score)(_ + _) >= max
      })

      solve(nextDowns, nextUps, events.tail)
    }
  }

  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextLine.toInt) {
      val events = mutable.SortedSet[Event]()
      for (j <- 0 until scanner.nextLine.toInt) {
        val note = Note(j, scanner.nextInt, scanner.nextInt, scanner.nextInt, scanner.nextInt)
        events += Event(note, true, start(note))
        events += Event(note, false, end(note))
        scanner.nextLine
      }
      val solution = solve(Set.empty, Set(Up(-1, -1, 0)), events)
      println(s"Case #${i}: ${solution}")
    }
  }
}
