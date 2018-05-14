import java.util.Scanner

import scala.collection.mutable


object Challenge06ButtonHero {

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

  def solve(lastTime: Float, statuses: Set[Status], events: mutable.SortedSet[Event]): Set[Status] = {
    // Filter empty downs and old ups
    var (filteredDown, filteredUp) = statuses.partition(_.isInstanceOf[Down])
    filteredDown = filteredDown.filter(_ match {
      case s: Down if s.pending.isEmpty => false
      case _ => true
    })
    var (lastUps, oldUps) = filteredUp.partition(_ match {
      // Filter empty up
      case s: Up if (s.up == lastTime) => true
      case _ => false
    })
    if (oldUps.nonEmpty) {
      oldUps = Set(oldUps.maxBy(_.score))
    }

    // Proceed
    val statuses2 = filteredDown ++ (lastUps ++ oldUps)
    if (events.isEmpty) {
      statuses2
    } else {
      val event = events.head
      solve(event.time,
        if (event.start) {
          // In the case of start, either join other button down at the same time
          // or convert previous button up in down
          var (downs, ups) = statuses2.partition(_.isInstanceOf[Down])
          if (!downs.exists(_.down == start(event.note))) {
            downs = downs + Down(event.time, 0, Set(event.note))
          } else {
            downs = downs.map(_ match {
              case x: Down if (x.down == event.start) => Down(x.down, x.score, x.pending + event.note)
              case x => x
            })
          }

          ups = ups.flatMap(_ match {
            case x: Up if (x.up < event.time) => Set[Status](x, Down(event.time, x.score, Set(event.note)))
            case x => Set[Status](x)
          })
          ups ++ downs
        } else {
          // Collect points on all started status
          statuses2.flatMap(_ match {
            case s: Down if (s.down == start(event.note)) => Set[Status](Down(s.down, s.score, s.pending - event.note), Up(s.down, event.time, s.score + event.note.points))
            case s: Up if ((s.down == start(event.note)) && (s.up == end(event.note))) => Set[Status](Up(s.down, s.up, s.score + event.note.points))
            case s => Set(s)
          })
        },
        events.tail
      )
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
      val solution = solve(-1, Set(Up(-1, -1, 0)), events).map(_.score).max
      println(s"Case #${i}: ${solution}")
    }
  }
}
