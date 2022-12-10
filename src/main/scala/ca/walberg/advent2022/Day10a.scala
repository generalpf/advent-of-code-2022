package ca.walberg.advent2022

import scala.io.Source

@main def Day10a(args: String*): Unit = {
  val source = Source.fromFile("data/day10.txt")
  val xValues: Seq[Int] = source.getLines().foldLeft(Seq(1))(
    (x: Seq[Int], line: String) =>
      line.substring(0, 4) match {
        case "noop" => x :+ x.last
        case "addx" =>
          val operand = line.substring(5).toInt
          x ++ Seq(x.last, x.last + operand)
      }
  )
  println((20 until 221 by 40).map(i => xValues(i - 1) * i).sum)
  source.close()
}
