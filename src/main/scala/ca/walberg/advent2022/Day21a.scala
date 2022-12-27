package ca.walberg.advent2022

import scala.io.Source

@main def Day21a(args: String*): Unit = {
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
          Monkey(m.group(1), Right(m.group(2), m.group(3).head, m.group(4)), None, None)
        case None =>
          numberRegex.findFirstMatchIn(line) match
            case Some(m) =>
              Monkey(m.group(1), Left(m.group(2).toLong), None, None)
        )
  )
  source.close()

  def populateChildren(monkey: Monkey): Unit =
    monkey.job match
      case Left(_) =>
      case Right((operand1, _, operand2)) =>
        monkey.leftChild = monkeys.find(_.id == operand1)
        monkey.rightChild = monkeys.find(_.id == operand2)
        populateChildren(monkey.leftChild.head)
        populateChildren(monkey.rightChild.head)

  // build the tree.
  val rootMonkey = monkeys.find(_.id == "root").head
  populateChildren(rootMonkey)

  // not the most efficient way to start at the leaves and ladder up but there
  // aren't an impossible number of monkeys and the tree isn't too deep.
  while (rootMonkey.job.isRight) {
    monkeys.foreach(m =>
      m.job match
        case Left(_) =>
        case Right((_, operator, _)) =>
          (m.leftChild.head.job, m.rightChild.head.job) match
            case (Left(operand1), Left(operand2)) =>
              val number = operator match
                case '+' => operand1 + operand2
                case '-' => operand1 - operand2
                case '*' => operand1 * operand2
                case '/' => operand1 / operand2
              m.job = Left(number)
              if (m.id == "root")
                println(s"root monkey's number is $number")
            case _ =>
    )
  }
}
