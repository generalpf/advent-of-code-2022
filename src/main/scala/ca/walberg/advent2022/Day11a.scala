package ca.walberg.advent2022

import scala.:+
import scala.io.Source

@main def Day11a(args: String*): Unit = {
  class Monkey(val items: Seq[Int], val operation: (old: Int) => Int, val divisor: Int, val trueTarget: Int, val falseTarget: Int, val inspectionCount: Int)

  val source = Source.fromFile("data/day11.txt")
  val operationRegex = """Operation: new = old (.) (.*)""".r
  val monkeys: Seq[Monkey] = source.getLines().grouped(7).foldLeft(Seq())(
    (acc: Seq[Monkey], lines: Seq[String]) =>
      val items = lines(1).substring(18).split(", ").map(_.toInt)
      val operation = operationRegex.findFirstMatchIn(lines(2)) match
        case Some(m) =>
          (old: Int) =>
            val operand = m.group(2) match
              case "old" => old
              case _ => m.group(2).toInt
            m.group(1).match
              case "+" => old + operand
              case "*" => old * operand
      val divisor = lines(3).substring(21).toInt
      val trueTarget = lines(4).substring(29).toInt
      val falseTarget = lines(5).substring(30).toInt
      acc :+ Monkey(items, operation, divisor, trueTarget, falseTarget, 0)
  )

  val finalMonkeys = (0 until 20).foldLeft(monkeys)(
    (acc: Seq[Monkey], _) => // each round (0 to 19)
      acc.indices.foldLeft(acc)(
        (acc: Seq[Monkey], thisMonkeyIndex: Int) => // each monkey inspects their items
          val thisMonkey = acc(thisMonkeyIndex)
          thisMonkey.items
            .foldLeft(acc)(
              (acc: Seq[Monkey], item: Int) => // for each one of this monkey's items
                val worry: Int = thisMonkey.operation(item) / 3
                val targetMonkeyIndex = if (worry % thisMonkey.divisor == 0) thisMonkey.trueTarget else thisMonkey.falseTarget
                val targetMonkey = acc(targetMonkeyIndex)
                acc.patch(targetMonkeyIndex, Seq(Monkey(targetMonkey.items :+ worry, targetMonkey.operation, targetMonkey.divisor, targetMonkey.trueTarget, targetMonkey.falseTarget, targetMonkey.inspectionCount)), 1)
            )
            .patch(thisMonkeyIndex, Seq(Monkey(Seq(), thisMonkey.operation, thisMonkey.divisor, thisMonkey.trueTarget, thisMonkey.falseTarget, thisMonkey.inspectionCount + thisMonkey.items.length)), 1)
      )
  )

  println(finalMonkeys.map(_.inspectionCount).sortBy(-_).take(2))
  source.close()
}
