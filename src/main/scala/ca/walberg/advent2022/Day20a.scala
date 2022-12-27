package ca.walberg.advent2022

import scala.io.Source

@main def Day20a(args: String*): Unit = {
  class Wint(val i: Int) extends AnyRef

  val source = Source.fromFile("data/day20.txt")
  val numbers: Seq[Wint] = source.getLines().foldLeft(Seq())(
    (acc: Seq[Wint], line: String) =>
      acc :+ Wint(line.toInt)
  )
  source.close()

  def mix(numbers: Seq[Wint]): Seq[Wint] =
    var newNumbers = numbers
    numbers.foreach(n =>
      n.i % (numbers.length - 1) match
        case 0 =>
        case _ =>
          val oldIndex = newNumbers.indexWhere(_ eq n)
          newNumbers = newNumbers.patch(oldIndex, Seq(), 1)
          val newIndex = (oldIndex + n.i) % newNumbers.length match
            case i if i < 0 => i + newNumbers.length
            case i => i
          newNumbers = newNumbers.take(newIndex) ++ Seq(n) ++ newNumbers.takeRight(newNumbers.length - newIndex)
    )
    newNumbers

  def code(numbers: Seq[Wint]): Int =
    val positionOfZero = numbers.indexWhere(_.i == 0)
    Seq(1000, 2000, 3000)
      .map(n => numbers((positionOfZero + n) % numbers.length))
      .map(n => n.i)
      .sum

  println(code(mix(numbers)))
}
