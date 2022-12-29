package ca.walberg.advent2022

import scala.collection.mutable
import scala.io.Source

@main def Day24a(args: String*): Unit = {
  val source = Source.fromFile("data/day24.txt")
  val startingValley: Array[String] = source.getLines()
    .map(line =>
      line
        .replace('>', '1')
        .replace('v', '2')
        .replace('<', '4')
        .replace('^', '8')
    )
    .toArray
  source.close()

  def valleyWidth = startingValley.head.length
  def valleyHeight = startingValley.length

  def adding(c: Char, direction: Int): Char =
    c match {
      case '.' => direction.toHexString.last
      case _ => (Integer.parseInt(c.toString, 16) | direction).toHexString.last
    }

  def advance(valley: Array[String]): Array[String] =
    val newValley: Array[String] = Array(valley.head) ++ Array.fill(valleyHeight - 2, valleyWidth - 2)('.').map(dots => s"#${String(dots)}#") ++ Array(valley.last)

    (1 until valleyHeight - 1).foreach(y =>
      (1 until valleyWidth - 1).foreach(x =>
        valley(y)(x) match {
          case '.' =>
          case 'E' =>
          case _ =>
            val foo = Integer.parseInt(valley(y)(x).toString, 16)
            if ((foo & 1) != 0) {
              // blizzard moving to the right
              if (valley(y)(x + 1) == '#')
                newValley(y) = newValley(y).updated(1, adding(newValley(y)(1), 1))
              else
                newValley(y) = newValley(y).updated(x + 1, adding(newValley(y)(x + 1), 1))
            }
            if ((foo & 4) != 0) {
              // blizzard moving to the left
              if (valley(y)(x - 1) == '#')
                newValley(y) = newValley(y).updated(valleyWidth - 2, adding(newValley(y)(valleyWidth - 2), 4))
              else
                newValley(y) = newValley(y).updated(x - 1, adding(newValley(y)(x - 1), 4))
            }
            if ((foo & 2) != 0) {
              // blizzard moving down
              if (valley(y + 1)(x) == '#')
                newValley(1) = newValley(1).updated(x, adding(newValley(1)(x), 2))
              else
                newValley(y + 1) = newValley(y + 1).updated(x, adding(newValley(y + 1)(x), 2))
            }
            if ((foo & 8) != 0) {
              // blizzard moving up
              if (valley(y - 1)(x) == '#')
                newValley(valleyHeight - 2) = newValley(valleyHeight - 2).updated(x, adding(newValley(valleyHeight - 2)(x), 8))
              else
                newValley(y - 1) = newValley(y - 1).updated(x, adding(newValley(y - 1)(x), 8))
            }
        }
      )
    )
    newValley

  var valleys: Seq[Array[String]] = Seq(startingValley)

  def valleyForMinute(minute: Int): Array[String] =
    if (minute < valleys.length)
      valleys(minute)
    else
      val valley = advance(valleyForMinute(minute - 1))
      valleys :+= valley
      valley

  class Space(val x: Int, val y: Int, val minutesAway: Int) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Space]
    override def equals(other: Any): Boolean = other match {
      case that: Space =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y &&
          minutesAway == that.minutesAway
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(x, y, minutesAway)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  def shortestPath(): Int =
    val queue = mutable.Queue[Space]()
    val explored = mutable.Queue[Space]()
    queue.append(Space(x = 1, y = 0, minutesAway = 0))
    while (queue.nonEmpty) {
      val e = queue.dequeue()
      val valley = valleyForMinute(e.minutesAway + 1)
      if (e.y == valleyHeight - 1) {
        return e.minutesAway
      }
      Seq(
        if (valley(e.y)(e.x - 1) == '.') Some((e.x - 1, e.y)) else None,
        if (valley(e.y)(e.x + 1) == '.') Some((e.x + 1, e.y)) else None,
        if (e.y > 0 && valley(e.y - 1)(e.x) == '.') Some((e.x, e.y - 1)) else None,
        if (valley(e.y + 1)(e.x) == '.') Some((e.x, e.y + 1)) else None,
        if (valley(e.y)(e.x) == '.') Some((e.x, e.y)) else None,
      )
        .flatten
        .map(coords => Space(x = coords._1, y = coords._2, minutesAway = e.minutesAway + 1))
        .filter(s => !explored.contains(s))
        .foreach(s =>
          explored.append(s)
          queue.enqueue(s)
        )
    }
    Int.MaxValue

  println(shortestPath())
}
