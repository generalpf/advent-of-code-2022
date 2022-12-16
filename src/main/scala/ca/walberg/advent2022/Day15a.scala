package ca.walberg.advent2022

import scala.io.Source

@main def Day15a(args: String*): Unit = {
  def manhattanDistance(fromX: Int, fromY: Int, toX: Int, toY: Int): Int = Math.abs(toX - fromX) + Math.abs(toY - fromY)

  val source = Source.fromFile("data/day15.txt")
  val regex = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  val sensorsAndBeacons: Seq[((Int, Int), (Int, Int))] = source.getLines().foldLeft(Seq())(
    (acc: Seq[((Int, Int), (Int, Int))], line: String) =>
      regex.findFirstMatchIn(line) match
        case Some(m) =>
          acc :+ ((m.group(1).toInt, m.group(2).toInt), (m.group(3).toInt, m.group(4).toInt))
  )
  val horizontalBounds = sensorsAndBeacons.map(_._1._1) ++ sensorsAndBeacons.map(_._2._1)
  val leftX = horizontalBounds.min
  val rightX = horizontalBounds.max
  val greatestMD = sensorsAndBeacons
    .map((sensor, beacon) => manhattanDistance(sensor._1, sensor._2, beacon._1, beacon._2))
    .max

  val y = 2000000
  val noBeaconHere = ((leftX - greatestMD) until (rightX + greatestMD + 1)).count(x =>
    // are there any sensors that are further from their beacon than they are to this point?
    sensorsAndBeacons.indexWhere((sensor, beacon) =>
      // is the distance from sensor -> beacon greater than or equal to the distance from sensor -> (x, y)?
      !(beacon._1 == x && beacon._2 == y) && manhattanDistance(sensor._1, sensor._2, beacon._1, beacon._2) >= manhattanDistance(sensor._1, sensor._2, x, y)
    ) != -1
  )

  println(noBeaconHere)
  source.close()
}
