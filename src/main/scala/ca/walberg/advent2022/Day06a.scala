package ca.walberg.advent2022

import scala.io.Source

@main def Day06a(args: String*): Unit = {
  val markerLength = 4
  val source = Source.fromFile("data/day6.txt")
  val signal = source.bufferedReader().readLine()
  val markerIndex = (0 until signal.length - markerLength).indexWhere(
    i => signal.substring(i, i + markerLength).toSet.size == markerLength
  )
  println(s"at ${markerIndex + markerLength} marker is ${signal.substring(markerIndex, markerIndex + markerLength)}")
  source.close()
}
