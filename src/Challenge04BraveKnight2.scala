import java.util.Scanner

import scala.collection.immutable.Queue

object Challenge04BraveKnight2 {

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

  case class Board (width: Int, height: Int, positions: String) {
    def index (pos: (Int, Int)): Int = pos._2 * width + pos._1
    def get (pos: (Int, Int)): Char = positions(index(pos))
    def which (c: Char) : (Int, Int) = {
      val i = positions.indexOf(c)
      (i % width, i / width)
    }
  }

  case class Status (steps: List[(Int, Int)], board: Board) {
    def dump: Unit = {
      for (i <- 0 until board.height) {
        for (j <- 0 until board.width) {
          val pos = (j, i)
          val c = if (steps.contains(pos)) 'V' else board.get(pos)
          print(c)
        }
        println
      }
      println
    }
  }

  def distance(v: (Int, Int), target: (Int, Int)) = Math.abs(target._1 - v._1) + Math.abs(target._2 - v._2)

  def paths (status: Status, target: (Int, Int)): Queue[Status] = {
    val start = status.steps.head
    val board = status.board
    // println("Start")
    val r = (if (status.board.get(start) == TRAMPOLINE) jumps else moves).map(move => {
      (start._1 + move.head, start._2 + move.tail.head)
    }).filter(p => {
      // Discard out of bounds
      val x = p._1
      val y = p._2
      !(x < 0 || y < 0 || x >= board.width || y >= board.height)
    }).filter(move => {
      if (status.steps.contains(move) || board.get(move) == LAVA)
        false
      else
        true
    }).sortWith((a, b) => {
      distance(a, target) <= distance(b, target)
    }).map(move => {
      Status(move :: status.steps, board)
    }).map(status => {
      // status.dump
      status
    })

    // println("Stop")

    /*
    println(s"Start from: ${status}")
    r.foreach(s => { println(s); s.dump })
    println("End")
    */
    r
  }

  // https://stackoverflow.com/a/41350143
  def p (move: Status, target: (Int, Int)): Stream[Status] = {
    def recurse (moves: Queue[Status]): Stream[Status] = {
      if (moves.isEmpty) {
        Stream.empty
      } else {
        val (move, tail) = moves.dequeue
        if (distance(move.steps.head, target) > 40) {
          // Over a long distance, depth first
          move #:: recurse(paths(move, target) ++ tail)
        } else {
          // Closer distance, breadth first
          move #:: recurse(tail ++ paths(move, target))
        }
      }
    }
    move #:: recurse(Queue.empty ++ paths(move, target))
  }

  def main (args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    for (i <- 1 to scanner.nextLine().toInt) {
      val height = scanner.nextInt()
      val width = scanner.nextInt()
      scanner.nextLine()
      val s = (for (j <- 1 to height) yield scanner.nextLine()).mkString
      val board = Board(width, height, s)
      val start = board.which(START)
      val princess = board.which(PRINCESS)
      val destination = board.which(DESTINATION)

      // println(s"$i $width $height $board ${start}")

      // Locate princess, then locate exit
      //val found = p(Stream(Status(List(start), board))).find(s => {
      val found = p(Status(List(start), board), princess).find(s => {
         // println(s"Testing PRINCESS $s")
        s.steps.head == princess
      }) flatMap(s => {
        // println(s"FOUND princess: $s")
        // p(Stream(Status(List(s.steps.head), board))).find(s2 => {
        p(Status(List(s.steps.head), board), destination).find(s2 => {
          // println(s"Testing EXIT $s")
          s2.steps.head == destination
        }).flatMap(s3 => Option((s, s3)))
      })

      // If found
      val r = found match {
        case Some(x) => {
          // println(s"Found exit: $x")
          (x._1.steps.length + x._2.steps.length - 2).toString
        }
        case None => "IMPOSSIBLE"
      }

      println(s"Case #$i: $r")

    }
  }
}
