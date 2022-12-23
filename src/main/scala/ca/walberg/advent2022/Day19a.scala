package ca.walberg.advent2022

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.io.Source

@main def Day19a(args: String*): Unit = {
  class Blueprint(val id: Int, val orePerOreRobot: Int, val orePerClayRobot: Int, val orePerObsidianRobot: Int, val clayPerObsidianRobot: Int, val orePerGeodeRobot: Int, val obsidianPerGeodeRobot: Int)

  val source = Source.fromFile("data/day19.txt")

  val regex = "^Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.$".r
  val blueprints: Seq[Blueprint] = source.getLines().foldLeft(Seq())(
    (acc: Seq[Blueprint], line: String) =>
      regex.findFirstMatchIn(line) match
        case Some(m) =>
          acc :+ Blueprint(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt, m.group(5).toInt, m.group(6).toInt, m.group(7).toInt)
  )

  def geodeMined(b: Blueprint, minutesLeft: Int)(ore: Int, clay: Int, obsidian: Int, geodes: Int)(oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int): Int =
    if (minutesLeft == 0)
      return geodes

    val canBuildOreRobot = ore >= b.orePerOreRobot
    val canBuildClayRobot = ore >= b.orePerClayRobot
    val canBuildObsidianRobot = ore >= b.orePerObsidianRobot && clay >= b.clayPerObsidianRobot
    val canBuildGeodeRobot = ore >= b.orePerGeodeRobot && obsidian >= b.obsidianPerGeodeRobot

    Seq(
      if (canBuildGeodeRobot) Some(geodeMined(b, minutesLeft - 1)(ore + oreRobots - b.orePerGeodeRobot, clay + clayRobots, obsidian + obsidianRobots - b.obsidianPerGeodeRobot, geodes + geodeRobots)(oreRobots, clayRobots, obsidianRobots, geodeRobots + 1)) else None,
      if (canBuildObsidianRobot) Some(geodeMined(b, minutesLeft - 1)(ore + oreRobots - b.orePerObsidianRobot, clay + clayRobots - b.clayPerObsidianRobot, obsidian + obsidianRobots, geodes + geodeRobots)(oreRobots, clayRobots, obsidianRobots + 1, geodeRobots)) else None,
      if (canBuildClayRobot) Some(geodeMined(b, minutesLeft - 1)(ore + oreRobots - b.orePerClayRobot, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots)(oreRobots, clayRobots + 1, obsidianRobots, geodeRobots)) else None,
      if (canBuildOreRobot) Some(geodeMined(b, minutesLeft - 1)(ore + oreRobots - b.orePerOreRobot, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots)(oreRobots + 1, clayRobots, obsidianRobots, geodeRobots)) else None,
      if (!canBuildGeodeRobot) Some(geodeMined(b, minutesLeft - 1)(ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geodes + geodeRobots)(oreRobots, clayRobots, obsidianRobots, geodeRobots)) else None,
    )
      .flatten
      .max

  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newWorkStealingPool())

  val futures = blueprints.map { b =>
    Future {
      val mined = geodeMined(b, 24)(0, 0, 0, 0)(1, 0, 0, 0)
      println(s"blueprint ${b.id} mined $mined geode")
      mined * b.id
    }
  }

  val qualityLevels = futures.map(f => Await.result(f, Duration.Inf))

  println(s"quality levels = $qualityLevels, sum = ${qualityLevels.sum}")
  source.close()
}
