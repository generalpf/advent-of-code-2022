package ca.walberg.advent2022

import scala.:+
import scala.io.Source

@main def Day11b(args: String*): Unit = {
  class Monkey(val items: Seq[Long], val operation: (old: Long) => Long, val divisor: Int, val trueTarget: Int, val falseTarget: Int, val inspectionCount: Int)

  val source = Source.fromFile("data/day11.txt")
  val operationRegex = """Operation: new = old (.) (.*)""".r
  val monkeys: Seq[Monkey] = source.getLines().grouped(7).foldLeft(Seq())(
    (acc: Seq[Monkey], lines: Seq[String]) =>
      val items = lines(1).substring(18).split(", ").map(_.toLong)
      val operation = operationRegex.findFirstMatchIn(lines(2)) match
        case Some(m) =>
          (old: Long) =>
            val operand = m.group(2) match
              case "old" => old
              case _ => m.group(2).toLong
            m.group(1).match
              case "+" => old + operand
              case "*" => old * operand
      val divisor = lines(3).substring(21).toInt
      val trueTarget = lines(4).substring(29).toInt
      val falseTarget = lines(5).substring(30).toInt
      acc :+ Monkey(items, operation, divisor, trueTarget, falseTarget, 0)
  )

  // we only need to keep track of the worry level modulo the product of all divisors
  val productOfDivisors = monkeys.map(_.divisor).foldLeft(1)(
    (acc: Int, divisor: Int) => acc * divisor
  )

  val finalMonkeys = (0 until 10000).foldLeft(monkeys)(
    (acc: Seq[Monkey], _) => // each round (0 to 9999)
      acc.indices.foldLeft(acc)(
        (acc: Seq[Monkey], thisMonkeyIndex: Int) => // each monkey inspects their items
          val thisMonkey = acc(thisMonkeyIndex)
          thisMonkey.items
            .foldLeft(acc)(
              (acc: Seq[Monkey], item: Long) => // for each one of this monkey's items
                val worry: Long = thisMonkey.operation(item)
                val targetMonkeyIndex = if (worry % thisMonkey.divisor == 0) thisMonkey.trueTarget else thisMonkey.falseTarget
                val targetMonkey = acc(targetMonkeyIndex)
                acc.patch(targetMonkeyIndex, Seq(Monkey(targetMonkey.items :+ (worry % productOfDivisors), targetMonkey.operation, targetMonkey.divisor, targetMonkey.trueTarget, targetMonkey.falseTarget, targetMonkey.inspectionCount)), 1)
            )
            .patch(thisMonkeyIndex, Seq(Monkey(Seq(), thisMonkey.operation, thisMonkey.divisor, thisMonkey.trueTarget, thisMonkey.falseTarget, thisMonkey.inspectionCount + thisMonkey.items.length)), 1)
      )
  )

  println(finalMonkeys.map(_.inspectionCount).sortBy(-_).take(2))
  source.close()
}
