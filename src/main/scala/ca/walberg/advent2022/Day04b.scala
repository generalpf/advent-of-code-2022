package ca.walberg.advent2022

import scala.io.Source

@main def Day04b(args: String*): Unit = {
  val source = Source.fromFile("data/day4.txt")
  val regex = """(\d+)-(\d+),(\d+)-(\d+)""".r
  val score: Int = source.getLines().foldRight(0)(
    (line: String, acc: Int) =>
      acc + (regex.findFirstMatchIn(line) match
        case Some(m) =>
          if ((m.group(1).toInt <= m.group(3).toInt && m.group(2).toInt >= m.group(3).toInt) || (m.group(3).toInt <= m.group(1).toInt && m.group(4).toInt >= m.group(1).toInt)) 1 else 0
      )
  )
  println(score)
  source.close()
}
