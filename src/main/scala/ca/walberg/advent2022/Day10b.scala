package ca.walberg.advent2022

import scala.io.Source

@main def Day10b(args: String*): Unit = {
  val source = Source.fromFile("data/day10.txt")
  source.getLines().foldLeft(Seq(1))(
    (x: Seq[Int], line: String) =>
      val cycle = x.size - 1
      val column = cycle % 40
      line.substring(0, 4) match {
        case "noop" =>
          drawPixel(column, x.last)
          x :+ x.last
        case "addx" =>
          val operand = line.substring(5).toInt
          drawPixel(column, x.last)
          drawPixel(column + 1, x.last)
          x ++ Seq(x.last, x.last + operand)
      }
  )
  source.close()
}

def drawPixel(column: Int, x: Int): Unit =
  if (column >= x - 1 && column <= x + 1)
    print('#')
  else
    print('.')
  if (column % 40 == 39)
    println()