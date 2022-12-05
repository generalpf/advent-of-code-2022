package ca.walberg.advent2022

import scala.io.Source

@main def Day03a(args: String*): Unit = {
  val source = Source.fromFile("data/day3.txt")
  val score: Int = source.getLines().foldLeft(0)(
    (acc: Int, line: String) =>
      val left = line.toCharArray.take(line.length / 2).toSet
      val right = line.toCharArray.takeRight(line.length / 2).toSet
      val intersection = left.intersect(right)
      val priority = if (intersection.head.isUpper)
        intersection.head - 64 + 26
      else
        intersection.head - 96
      acc + priority
  )
  println(score)
  source.close()
}
