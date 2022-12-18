package ca.walberg.advent2022

import scala.io.Source

@main def Day16a(args: String*): Unit = {
  class Valve(val id: String, val flowRate: Int, val tunnels: String, var connections: Seq[Valve]):
    override def toString: String = s"valve $id"

  val source = Source.fromFile("data/day16.txt")
  val regex = """Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnel(s)? lead(s)? to valve(s)? (.*)""".r
  val valves: Seq[Valve] = source.getLines().filter(_.nonEmpty).foldLeft(Seq())(
    (acc: Seq[Valve], line: String) =>
      acc :+ (regex.findFirstMatchIn(line) match
        case Some(m) =>
          Valve(m.group(1), m.group(2).toInt, m.group(6), Seq())
      )
  )
  valves.foreach(v =>
    v.connections = v.tunnels
      .split(", ")
      .map(t =>
        valves.filter(v2 => v2.id == t).head
      )
  )

  var pathNetworkCache: Map[Valve, Map[Valve, Int]] = Map()
  def pathNetwork(from: Valve): Map[Valve, Int] =
    pathNetworkCache.get(from) match
      case Some(map) =>
        map
      case None =>
        var valveDistances: Map[Valve, (Boolean, Int)] = valves.map(v => if (v == from) v -> (true, 0) else v -> (false, Int.MaxValue)).toMap
        var currentValve = from
        while (true) {
          val thisDistance = valveDistances(currentValve)._2
          currentValve.connections.foreach(v =>
            if (!valveDistances(v)._1)
              val bestDistance = Math.min(valveDistances(v)._2, thisDistance + 1)
              valveDistances = valveDistances.updated(v, (false, bestDistance))
          )
          valveDistances = valveDistances.updated(currentValve, (true, thisDistance))
          valveDistances
            .filterNot(_._2._1)
            .minByOption(_._2._2) match
            case Some((valve, _)) => currentValve = valve
            case None =>
              val pathNetwork = valveDistances.map(_ -> _._2)
              pathNetworkCache = pathNetworkCache.updated(from, pathNetwork)
              return pathNetwork
        }
        Map()

  def bestPressureReleased(ptr: Valve, openValves: Seq[Valve], minutesLeft: Int, pressureReleased: Int): Int =
    val network = pathNetwork(ptr)
    valves                                      // for each valve
      .filter(v => v.flowRate > 0)              // with a positive flow rate
      .filterNot(v => v == ptr)                 // that isn't this one
      .filterNot(v => openValves.contains(v))   // and is still closed
      .filter(v => minutesLeft > network(v))    // and we have time to move to and open
      .map(v =>
        bestPressureReleased(v, openValves :+ v, minutesLeft - network(v) - 1, pressureReleased + (minutesLeft - network(v) - 1) * v.flowRate)
      )
      .maxOption
      .getOrElse(pressureReleased)

  val pressureReleased = bestPressureReleased(valves.find(_.id == "AA").head, Seq(), 30, 0)

  println(pressureReleased)
  source.close()
}
