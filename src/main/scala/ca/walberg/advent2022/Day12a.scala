package ca.walberg.advent2022

import scala.io.Source

@main def Day12a(args: String*): Unit = {
  class Point(val x: Int, val y: Int):
    def canEqual(other: Any): Boolean = other.isInstanceOf[Point]
    override def equals(other: Any): Boolean = other match {
      case that: Point =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y
      case _ => false
    }
    override def hashCode(): Int = {
      val state = Seq(x, y)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
    override def toString: String = s"($x, $y)"

  val source = Source.fromFile("data/day12.txt")
  val hill: Array[Array[Byte]] = source.getLines().foldLeft(Array[Array[Byte]]())(
    (acc: Array[Array[Byte]], line: String) =>
      acc :+ line.getBytes
  )

  def traversable(from: Point, to: Point): Boolean =
    val fromByte = if (hill(from.y)(from.x) == 'S') Byte.MaxValue else hill(from.y)(from.x)
    val toByte = if (hill(to.y)(to.x) == 'E') 'z'.toByte else hill(to.y)(to.x)
    fromByte > toByte || fromByte == toByte || fromByte == toByte - 1

  val start = Point(0, 20)

  assert(hill(start.y)(start.x) == 'S'.toByte)

  var nodes: Map[Point, (Boolean, Int)] = Map()
  hill.indices.foreach(
    y => hill(y).indices.foreach(
      x => nodes += (Point(x, y) -> (false, Int.MaxValue))
    )
  )

  nodes = nodes.updated(start, (false, 0))
  var ptr = start

  while (hill(ptr.y)(ptr.x) != 'E') {
    val distanceToPtr = nodes.getOrElse(ptr, (false, Int.MaxValue))._2
    val up = if (ptr.y > 0 && traversable(ptr, Point(ptr.x, ptr.y - 1))) Some(Point(ptr.x, ptr.y - 1)) else None
    val down = if (ptr.y < hill.length - 1 && traversable(ptr, Point(ptr.x, ptr.y + 1))) Some(Point(ptr.x, ptr.y + 1)) else None
    val left = if (ptr.x > 0 && traversable(ptr, Point(ptr.x - 1, ptr.y))) Some(Point(ptr.x - 1, ptr.y)) else None
    val right = if (ptr.x < hill(0).length - 1 && traversable(ptr, Point(ptr.x + 1, ptr.y))) Some(Point(ptr.x + 1, ptr.y)) else None
    Seq(up, down, left, right)
      .flatten
      .filterNot(nodes(_)._1)
      .foreach(point =>
        val distanceToPoint = nodes.getOrElse(point, (false, Int.MaxValue))._2
        nodes = nodes.updated(point, (false, Math.min(distanceToPoint, distanceToPtr) + 1))
      )
    nodes = nodes.updated(ptr, (true, distanceToPtr))
    ptr = nodes
      .filterNot(_._2._1)
      .minBy(_._2._2)
      ._1
  }

  println(nodes(ptr)._2)
  source.close()
}
