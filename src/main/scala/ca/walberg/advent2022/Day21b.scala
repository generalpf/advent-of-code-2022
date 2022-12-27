package ca.walberg.advent2022

import scala.io.Source

@main def Day21b(args: String*): Unit = {
  class Monkey(val id: String, var job: Either[Long, (String, Char, String)], var leftChild: Option[Monkey], var rightChild: Option[Monkey]):
    override def toString: String =
      val jobString = job match
        case Left(number) => number.toString
        case Right((operand1, operator, operand2)) => s"$operand1 $operator $operand2"
      s"$id: $jobString"

  val source = Source.fromFile("data/day21.txt")
  val operationRegex = """(.{4}): (.{4}) (.) (.{4})""".r
  val numberRegex = """(.{4}): (\d+)""".r
  val monkeys: Seq[Monkey] = source.getLines().foldLeft(Seq())(
    (acc: Seq[Monkey], line: String) =>
      acc :+ (operationRegex.findFirstMatchIn(line) match
        case Some(m) =>
          Monkey(m.group(1), Right(m.group(2), if (m.group(1) == "root") '=' else m.group(3).head, m.group(4)), None, None)
        case None =>
          numberRegex.findFirstMatchIn(line) match
            case Some(m) =>
              Monkey(m.group(1), Left(m.group(2).toLong), None, None)
        )
  )
  source.close()

  def populateChildren(monkey: Monkey, monkeys: Seq[Monkey]): Unit =
    monkey.job match
      case Left(_) =>
      case Right((operand1, _, operand2)) =>
        monkey.leftChild = monkeys.find(_.id == operand1)
        monkey.rightChild = monkeys.find(_.id == operand2)
        populateChildren(monkey.leftChild.head, monkeys)
        populateChildren(monkey.rightChild.head, monkeys)

  def equation(monkey: Monkey): String =
    monkey.job match
      case Left(value) =>
        monkey.id match
          case "humn" => "h"
          case _ => value.toString
      case Right((_, operand, _)) if operand == '-' || operand == '+' =>
        s"(${equation(monkey.leftChild.head)} $operand ${equation(monkey.rightChild.head)})"
      case Right((_, operand, _)) =>
        s"${equation(monkey.leftChild.head)} $operand ${equation(monkey.rightChild.head)}"

  val rootMonkey = monkeys.find(_.id == "root").head
  populateChildren(rootMonkey, monkeys)

  // plug this into something like Octave and have it solve for `h`.
  // it emits f(h) = g(h) so rearrange it to f(h) - g(h) = 0 and solve for `h`.
  println(equation(rootMonkey))
}
