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
    numbers.foldLeft(numbers)(
      (acc: Seq[Wint], n: Wint) =>
        n.i % (numbers.length - 1) match
          case 0 => acc
          case _ =>
            val oldIndex = acc.indexWhere(_ eq n)
            val newAcc = acc.patch(oldIndex, Seq(), 1)
            val newIndex = (oldIndex + n.i) % newAcc.length match
              case i if i < 0 => i + newAcc.length
              case i => i
            newAcc.take(newIndex) ++ Seq(n) ++ newAcc.takeRight(newAcc.length - newIndex)
    )

  def code(numbers: Seq[Wint]): Int =
    val positionOfZero = numbers.indexWhere(_.i == 0)
    Seq(1000, 2000, 3000)
      .map(n => numbers((positionOfZero + n) % numbers.length))
      .map(n => n.i)
      .sum

  println(code(mix(numbers)))
}
