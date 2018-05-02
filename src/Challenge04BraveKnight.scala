import java.util.Scanner

import scala.collection.immutable.Queue
import scala.collection.mutable

object Challenge04BraveKnight {

  val moves = for (
    deltaX <- Queue(-1, 1);
    deltaY <- Queue(-1, 1);
    swap <- Queue(false, true)
  ) yield {
    val r = List(1 * deltaX, 2 * deltaY)
    if (swap) r.reverse else r
  }

  val jumps = moves.map(_.map(_ * 2))

  final val LAVA = '#'
  final val START = 'S'
  final val PRINCESS = 'P'
  final val DESTINATION = 'D'
  final val GROUND = '.'
  final val TRAMPOLINE = '*'

  case class Board[T] (width: Int, height: Int, positions: IndexedSeq[T]) {
    def index (pos: (Int, Int)): Int = pos._2 * width + pos._1
    def position (index: Int): (Int, Int) = (index % width, index / width)
    def get (pos: (Int, Int)): T = positions(index(pos))
    def which (c: T) : (Int, Int) = position(positions.indexOf(c))
  }

  case class Distance (val index: Int, val distance: Int = Int.MaxValue, val processed: Boolean = false) extends Ordered[Distance] {
    def compare (that: Distance) = {
      val d = this.distance - that.distance
      if (d != 0)
        d
      else
        this.index - that.index
    }
  }

  /**
    * Dijsktra stopping at target
    */
  def solve (board: Board[Char], start: (Int, Int), target: (Int, Int)): Option[Int] = {
    // Init structures
    val length = (board.width * board.height)
    val array = new Array[Distance](length)
    val pending = mutable.SortedSet[Distance]()
    for (i <- 0 until length) {
      val cell = Distance(i)
      array(i) = cell
    }

    def index (pos: (Int, Int)) = board.index(pos)

    def get (pos: (Int, Int)): Distance = array(index(pos))

    def replace (d: Distance) = {
      val old = array(d.index)
      array(d.index) = d
      if (!old.processed) {
        pending.remove(old)
        pending += d
      }
    }

    replace(Distance(index(start), 0, false))

    // While not reached target and pending targets
    while (pending.nonEmpty && (pending.head.distance != Int.MaxValue) && !array(index(target)).processed) {
      val current = pending.head
      paths(board.position(current.index), board).filter(move => {
        !get(move).processed
      }).map(move => {
        // Use empty pending to distinguish the case target has been visited
        if (pending.nonEmpty) {
          val newDistance = current.distance + 1
          val oldDistance = array(board.index(move))
          if (oldDistance.distance > newDistance) {
            replace(Distance(oldDistance.index, newDistance, false))
          }

          // If move is to target, finish
          if (move == target) {
            pending.clear()
          }
        }
      })

      val newCurrent = Distance(current.index, current.distance, true)
      replace(newCurrent)
      pending.remove(newCurrent)
    }

    // Check target
    val r = get(target).distance
    if (r == Int.MaxValue) None else Some(r)
  }

  def paths (start: (Int, Int), board: Board[Char]): Queue[(Int, Int)] = {
    (if (board.get(start) == TRAMPOLINE) jumps else moves).map(move => {
      (start._1 + move.head, start._2 + move.tail.head)
    }).filter(p => {
      // Discard out of bounds
      val x = p._1
      val y = p._2
      !(x < 0 || y < 0 || x >= board.width || y >= board.height)
    }).filter(board.get(_) != LAVA)
  }

  def main (args: Array[String]): Unit = {
    val impossible = "IMPOSSIBLE"
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextLine().toInt) {
      val height = scanner.nextInt()
      val width = scanner.nextInt()
      scanner.nextLine()
      val s = (for (j <- 1 to height) yield scanner.nextLine()).mkString
      val board = Board(width, height, s)
      val r = solve(board, board.which(START), board.which(PRINCESS)) match {
        case Some(x) => {
          solve(board, board.which(PRINCESS), board.which(DESTINATION)) match {
            case Some (y) => {
              (x + y).toString
            }
            case _ => impossible // Destination unreachable
          }
        }
        case _ => impossible // Princess unreachable
      }
      println(s"Case #$i: $r")

    }
  }
}
