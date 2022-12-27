package ca.walberg.advent2022

import scala.io.Source

@main def Day20b(args: String*): Unit = {
  class Wlint(val l: Long) extends AnyRef

  val source = Source.fromFile("data/day20.txt")
  val numbers: Seq[Wlint] = source.getLines().foldLeft(Seq())(
    (acc: Seq[Wlint], line: String) =>
      acc :+ Wlint(line.toLong * 811589153L)
  )
  source.close()

  def mix(numbers: Seq[Wlint], times: Int): Seq[Wlint] =
    var newNumbers = numbers
    (0 until times).foreach(_ =>
      numbers.foreach(n =>
        n.l % (numbers.length - 1) match
          case 0 =>
          case _ =>
            val oldIndex = newNumbers.indexWhere(_ eq n)
            newNumbers = newNumbers.patch(oldIndex, Seq(), 1)
            val newIndex = (oldIndex + n.l) % newNumbers.length match
              case i if i < 0 => i + newNumbers.length
              case i => i
            newNumbers = newNumbers.take(newIndex.toInt) ++ Seq(n) ++ newNumbers.takeRight(newNumbers.length - newIndex.toInt)
      )
    )
    newNumbers

  def code(numbers: Seq[Wlint]): Long =
    val positionOfZero = numbers.indexWhere(_.l == 0)
    Seq(1000, 2000, 3000)
      .map(n => numbers((positionOfZero + n) % numbers.length))
      .map(n => n.l)
      .sum

  println(code(mix(numbers, 10)))
}
