package ca.walberg.advent2022

import scala.io.Source

@main def Day03b(args: String*): Unit = {
  val source = Source.fromFile("data/day3.txt")
  val score: Int = source.getLines().grouped(3).toList.foldRight(0)(
    (group: Seq[String], acc: Int) =>
      val intersection = group(0).toSet.intersect(group(1).toSet).intersect(group(2).toSet)
      val priority = if (intersection.head.isUpper)
        intersection.head - 64 + 26
      else
        intersection.head - 96
      acc + priority
  )
  println(score)
  source.close()
}
