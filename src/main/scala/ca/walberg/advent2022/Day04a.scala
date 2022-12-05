package ca.walberg.advent2022

import scala.io.Source

@main def Day04a(args: String*): Unit = {
  val source = Source.fromFile("data/day4.txt")
  val regex = """(\d+)-(\d+),(\d+)-(\d+)""".r
  val score: Int = source.getLines().foldLeft(0)(
    (acc: Int, line: String) =>
      acc + (regex.findFirstMatchIn(line) match
        case Some(m) =>
          if ((m.group(1).toInt <= m.group(3).toInt && m.group(2).toInt >= m.group(4).toInt) || (m.group(3).toInt <= m.group(1).toInt && m.group(4).toInt >= m.group(2).toInt)) 1 else 0
      )
  )
  println(score)
  source.close()
}
