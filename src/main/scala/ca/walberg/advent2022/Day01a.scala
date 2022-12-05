package ca.walberg.advent2022

import scala.io.Source

@main def Day01a(args: String*): Unit = {
  val source = Source.fromFile("data/day1.txt")
  val caloriesPer: Seq[Int] = source.getLines().foldLeft(Seq(0))(
    (acc: Seq[Int], line: String) =>
      line match
        case "" => acc :+ 0
        case _ => acc.dropRight(1) :+ (acc.last + line.toInt)
  )
  println(caloriesPer.max)
  source.close()
}
