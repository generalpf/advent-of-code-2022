package ca.walberg.advent2022

import java.io.{BufferedReader, File, FileReader}
import scala.io.Source
@main def Day02b(args: String*): Unit = {
    val source = Source.fromFile("data/day2.txt")
    val score: Int = source.getLines().foldRight(0)(
      (line: String, acc: Int) =>
        acc + (line match
          case "A X" => 0 + 3
          case "A Y" => 3 + 1
          case "A Z" => 6 + 2
          case "B X" => 0 + 1
          case "B Y" => 3 + 2
          case "B Z" => 6 + 3
          case "C X" => 0 + 2
          case "C Y" => 3 + 3
          case "C Z" => 6 + 1
    ))
    println(score)
    source.close()
}