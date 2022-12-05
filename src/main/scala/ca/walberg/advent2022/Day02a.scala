package ca.walberg.advent2022

import scala.io.Source

@main def Day02a(args: String*): Unit = {
  val source = Source.fromFile("data/day2.txt")
  val score: Int = source.getLines().foldLeft(0)(
    (acc: Int, line: String) =>
      acc + (line match
        case "A X" => 3 + 1
        case "A Y" => 6 + 2
        case "A Z" => 0 + 3
        case "B X" => 0 + 1
        case "B Y" => 3 + 2
        case "B Z" => 6 + 3
        case "C X" => 6 + 1
        case "C Y" => 0 + 2
        case "C Z" => 3 + 3
  ))
  println(score)
  source.close()
}
