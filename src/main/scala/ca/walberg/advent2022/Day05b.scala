package ca.walberg.advent2022

import scala.io.Source

@main def Day05b(args: String*): Unit = {
  val piles = Seq(
    Seq('M', 'J', 'C', 'B', 'F', 'R', 'L', 'H'),
    Seq('Z', 'C', 'D'),
    Seq('H', 'J', 'F', 'C', 'N', 'G', 'W'),
    Seq('P', 'J', 'D', 'M', 'T', 'S', 'B'),
    Seq('N', 'C', 'D', 'R', 'J'),
    Seq('W', 'L', 'D', 'Q', 'P', 'J', 'G', 'Z'),
    Seq('P', 'Z', 'T', 'F', 'R', 'H'),
    Seq('L', 'V', 'M', 'G'),
    Seq('C', 'B', 'G', 'P', 'F', 'Q', 'R', 'J'),
  )

  val source = Source.fromFile("data/day5.txt")
  val regex = """move (\d+) from (\d) to (\d+)""".r
  val finalPiles: Seq[Seq[Char]] = source.getLines().foldLeft(piles)(
    (acc: Seq[Seq[Char]], line: String) =>
      regex.findFirstMatchIn(line) match
        case Some(m) =>
          val quantity = m.group(1).toInt
          val fromPileIndex = m.group(2).toInt - 1
          val toPileIndex = m.group(3).toInt - 1
          val crates = acc(fromPileIndex).takeRight(quantity)
          acc.patch(fromPileIndex, Seq(acc(fromPileIndex).dropRight(quantity)), 1).patch(toPileIndex, Seq(acc(toPileIndex) ++ crates), 1)
        case None => acc // starting configuration line; ignore.
  )
  println(finalPiles.map(_.last))
  source.close()
}
