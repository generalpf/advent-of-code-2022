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
    (0 until times).foldLeft(numbers)(
      (acc: Seq[Wlint], _: Int) =>
        numbers.foldLeft(acc)(
          (acc: Seq[Wlint], n: Wlint) =>
            n.l % (numbers.length - 1) match
              case 0 => acc
              case _ =>
                val oldIndex = acc.indexWhere(_ eq n)
                val newAcc = acc.patch(oldIndex, Seq(), 1)
                val newIndex = (oldIndex + n.l) % newAcc.length match
                  case i if i < 0 => i + newAcc.length
                  case i => i
                newAcc.take(newIndex.toInt) ++ Seq(n) ++ newAcc.takeRight(newAcc.length - newIndex.toInt)
        )
    )

  def code(numbers: Seq[Wlint]): Long =
    val positionOfZero = numbers.indexWhere(_.l == 0)
    Seq(1000, 2000, 3000)
      .map(n => numbers((positionOfZero + n) % numbers.length))
      .map(n => n.l)
      .sum

  println(code(mix(numbers, 10)))
}
