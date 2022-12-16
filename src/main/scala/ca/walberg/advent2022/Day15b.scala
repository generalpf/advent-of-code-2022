package ca.walberg.advent2022

import scala.io.Source

@main def Day15b(args: String*): Unit = {
  def manhattanDistance(fromX: Int, fromY: Int, toX: Int, toY: Int): Int = Math.abs(toX - fromX) + Math.abs(toY - fromY)

  val source = Source.fromFile("data/day15.txt")
  val regex = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  val sensorsAndBeacons: Seq[((Int, Int), (Int, Int))] = source.getLines().foldLeft(Seq())(
    (acc: Seq[((Int, Int), (Int, Int))], line: String) =>
      regex.findFirstMatchIn(line) match
        case Some(m) =>
          acc :+ ((m.group(1).toInt, m.group(2).toInt), (m.group(3).toInt, m.group(4).toInt))
  )

  val dimension = 4000000

  (0 until dimension + 1).indexWhere(y =>
    var x = 0
    var found = false
    while (!found && x <= dimension) {
      // find first sensor that we're within matching beacon range of.
      val inRangeOf = sensorsAndBeacons.indexWhere((sensor, beacon) =>
        // is the distance from sensor -> beacon greater than or equal to the distance from sensor -> (x, y)?
        manhattanDistance(sensor._1, sensor._2, beacon._1, beacon._2) >= manhattanDistance(sensor._1, sensor._2, x, y)
      )
      if (inRangeOf == -1) {
        println(s"($x, $y) => ${BigInt(x) * 4000000 + y}")
        found = true
      } else {
        val sensorAndBeacon = sensorsAndBeacons(inRangeOf)
        val distanceFromSensorToBeacon = manhattanDistance(sensorAndBeacon._1._1, sensorAndBeacon._1._2, sensorAndBeacon._2._1, sensorAndBeacon._2._2)
        x = sensorAndBeacon._1._1 + (distanceFromSensorToBeacon - Math.abs(sensorAndBeacon._1._2 - y)) + 1
      }
    }
    found
  )

  source.close()
}
