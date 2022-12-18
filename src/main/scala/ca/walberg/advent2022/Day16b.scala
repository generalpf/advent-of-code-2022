package ca.walberg.advent2022

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.io.Source

@main def Day16b(args: String*): Unit = {
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
        Map()   // does not happen.

  def intermediaryValve(from: Valve, to: Valve, steps: Int): Valve =
    // use the dijkstra map backwards.
    val map = pathNetwork(from)
    var ptr = to
    // until we've moved enough steps, keep moving one closer to the start.
    while (map(ptr) != steps) {
      ptr = ptr
        .connections
        .find(v => map(v) == map(ptr) - 1)
        .head
    }
    ptr

  def bestPressureReleased(myPtr: Valve, elephantPtr: Valve, openValves: Seq[Valve], minutesLeft: Int, pressureReleased: Int): Int =
    val myNetwork = pathNetwork(myPtr)
    val elephantNetwork = pathNetwork(elephantPtr)
    val goodValves = valves
      .filter(v => v.flowRate > 0)              // with a positive flow rate
      .filterNot(v => openValves.contains(v))   // and is still closed
    val valvesForMe = goodValves
      .filter(v => minutesLeft > myNetwork(v))          // and i have time to move to and open
      .filter(v => myNetwork(v) <= elephantNetwork(v))  // and i am closer (or the same distance) than the elephant
    val valvesForElephant = goodValves
      .filter(v => minutesLeft > elephantNetwork(v))    // and it has time to move to and open
      .filter(v => elephantNetwork(v) <= myNetwork(v))  // and it is closer (or the same distance) than me
    (valvesForMe, valvesForElephant) match
      case (Nil, Nil) =>
        pressureReleased
      case (Nil, ve) =>
        // i have nowhere to go.
        ve
          .map(e => bestPressureReleased(myPtr, e, openValves :+ e, minutesLeft - elephantNetwork(e) - 1, pressureReleased + (minutesLeft - elephantNetwork(e) - 1) * e.flowRate))
          .max
      case (vm, Nil) =>
        // the elephant has nowhere to go.
        vm
          .map(m => bestPressureReleased(m, elephantPtr, openValves :+ m, minutesLeft - myNetwork(m) - 1, pressureReleased + (minutesLeft - myNetwork(m) - 1) * m.flowRate))
          .max
      case _ =>
        valvesForMe
          .flatMap(vm => valvesForElephant.map(ve => (vm, ve)))       // produce a product of these two sequences as tuples
          .filterNot((m, e) => m == e)                                // but not where they're the same destination
          .filterNot((m, e) => myPtr == elephantPtr && valvesForElephant.contains(m) && valvesForMe.contains(e) && m.id > e.id)   // and if we're in the same location, ensure m < e
          .map((m, e) =>
            if (openValves.isEmpty)
              println(s"${java.util.Date()}: root decision, evaluating ($m, $e) of ${valvesForMe.length * valvesForElephant.length}")
            if (myNetwork(m) == elephantNetwork(e)) {
              // we'd get there at the same time.
              bestPressureReleased(m, e, openValves :+ m :+ e, minutesLeft - myNetwork(m) - 1, pressureReleased + (minutesLeft - myNetwork(m) - 1) * (m.flowRate + e.flowRate))
            } else if (myNetwork(m) < elephantNetwork(e)) {
              // i'd get there first.
              val eMoveTo = intermediaryValve(elephantPtr, e, Math.min(myNetwork(m) + 1, minutesLeft))  // extra minute while the i open the valve
              bestPressureReleased(m, eMoveTo, openValves :+ m, minutesLeft - myNetwork(m) - 1, pressureReleased + (minutesLeft - myNetwork(m) - 1) * m.flowRate)
            } else {
              // it would get there first.
              val mMoveTo = intermediaryValve(myPtr, m, Math.min(elephantNetwork(e) + 1, minutesLeft))  // extra minute while the it opens the valve
              bestPressureReleased(mMoveTo, e, openValves :+ e, minutesLeft - elephantNetwork(e) - 1, pressureReleased + (minutesLeft - elephantNetwork(e) - 1) * e.flowRate)
            }
          )
          .maxOption
          .getOrElse(pressureReleased)

  val AA = valves.find(_.id == "AA").head

  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newWorkStealingPool())

  val goodValves = valves
    .filter(v => v.flowRate > 0) // with a positive flow rate

  // each of my valves is using a separate thread.
  val minutesLeft = 26
  val futures = goodValves.map(m =>
    val network = pathNetwork(AA)
    Future[Option[Int]] {
      goodValves
        .filter(e => m.id < e.id)
        .map(e =>
          if (network(m) == network(e)) {
            // we'd get there at the same time.
            bestPressureReleased(m, e, Seq(m, e), minutesLeft - network(m) - 1, (minutesLeft - network(m) - 1) * (m.flowRate + e.flowRate))
          } else if (network(m) < network(e)) {
            // i'd get there first.
            val eMoveTo = intermediaryValve(AA, e, Math.min(network(m) + 1, minutesLeft))  // extra minute while the i open the valve
            bestPressureReleased(m, eMoveTo, Seq(m), minutesLeft - network(m) - 1, (minutesLeft - network(m) - 1) * m.flowRate)
          } else {
            // it would get there first.
            val mMoveTo = intermediaryValve(AA, m, Math.min(network(e) + 1, minutesLeft))  // extra minute while the it opens the valve
            bestPressureReleased(mMoveTo, e, Seq(e), minutesLeft - network(e) - 1, (minutesLeft - network(e) - 1) * e.flowRate)
          }
        )
        .maxOption
    }
  )

  val bestResult = futures
    .map(f => Await.result(f, Duration.Inf))
    .max

  println(bestResult)

  source.close()
}
