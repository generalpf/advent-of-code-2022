package ca.walberg.advent2022

import scala.io.Source

@main def Day09a(args: String*): Unit = {
  class Point(val x: Int, val y: Int):
    def distanceTo(p: Point): Double = Math.sqrt(Math.pow(this.x - p.x, 2.0) + Math.pow(this.y - p.y, 2.0))
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

  class State(val h: Point, val t: Point, val trail: Set[Point])

  val source = Source.fromFile("data/day9.txt")
  val finalState: State = source.getLines().foldLeft(State(Point(0, 0), Point(0, 0), Set()))(
    (state: State, line: String) =>
      (0 until line.substring(2).toInt).foldLeft(state)(
        (state: State, _: Int) =>
          val newHead = line.head match
            case 'U' => Point(state.h.x, state.h.y + 1)
            case 'D' => Point(state.h.x, state.h.y - 1)
            case 'L' => Point(state.h.x - 1, state.h.y)
            case 'R' => Point(state.h.x + 1, state.h.y)
          val newTail = if (newHead.distanceTo(state.t) >= 2.0)
            Point(state.t.x + Math.signum(newHead.x - state.t.x).toInt, state.t.y + Math.signum(newHead.y - state.t.y).toInt)
          else
            state.t
          State(newHead, newTail, state.trail + newTail)
      )
  )

  println(s"head = ${finalState.h}, tail = ${finalState.t}, visited ${finalState.trail.size} locations (including origin)")
  source.close()
}
