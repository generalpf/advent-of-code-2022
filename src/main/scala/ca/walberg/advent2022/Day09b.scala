package ca.walberg.advent2022

import scala.io.Source

@main def Day09b(args: String*): Unit = {
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

  class State(val knots: Seq[Point], val trail: Set[Point])

  val source = Source.fromFile("data/day9.txt")
  val finalState: State = source.getLines().foldLeft(State(Seq(Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0), Point(0, 0)), Set()))(
    (state: State, line: String) =>
      (0 until line.substring(2).toInt).foldLeft(state)(
        (state: State, _: Int) =>
          val newHead = line.head match
            case 'U' => Point(state.knots.head.x, state.knots.head.y + 1)
            case 'D' => Point(state.knots.head.x, state.knots.head.y - 1)
            case 'L' => Point(state.knots.head.x - 1, state.knots.head.y)
            case 'R' => Point(state.knots.head.x + 1, state.knots.head.y)
          val newKnots = state.knots.drop(1).scanLeft(newHead)(
            (prev, knot) =>
              if (prev.distanceTo(knot) >= 2.0)
                Point(knot.x + Math.signum(prev.x - knot.x).toInt, knot.y + Math.signum(prev.y - knot.y).toInt)
              else
                knot
          )
          State(newKnots, state.trail + newKnots.last)
      )
  )

  println(s"knots = ${finalState.knots}, visited ${finalState.trail.size} locations (including origin)")
  source.close()
}
