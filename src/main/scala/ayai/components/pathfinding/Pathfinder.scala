package ayai.components.pathfinding

import java.util

import ayai.components.Position
import ayai.factories.{NodeState, GraphFactory, Node}
import crane.Component
import java.util.TreeSet

abstract class Pathfinder(heuristic: DistanceHeuristic) extends Component {
  type Matrix2D = Array[Array[Node]]

  def findPath(map: Matrix2D, start: Position, end: Position): Option[List[Position]]
}

/**
 * Pathfinding component which implements the A* pathfinding algorithm
 *
 * http://en.wikipedia.org/wiki/A*_search_algorithm
 *
 * @param heuristic Instance of DistanceHeuristic to measure distance during A*
 */
class AStar(heuristic: DistanceHeuristic) extends Pathfinder(heuristic) {
  def findPath(map: Matrix2D, start: Position, end: Position): Option[List[Position]] = {
    val startFScore = heuristic.estimateDistance(start, end)
    val closedSet = new java.util.PriorityQueue[WeightedNode]()
    val openSet = new util.PriorityQueue[WeightedNode]()

    val cameFrom = new collection.mutable.HashMap[Position, Option[Position]]()
    cameFrom.update(start, None)

    val costSoFar = new collection.mutable.HashMap[Position, Long]()
    costSoFar.update(start, 0)

    val startNode = WeightedNode(start, 0)
    openSet.add(startNode)

    var current = startNode

    while (!openSet.isEmpty) {
      current = openSet.poll()

      if (current.position == end) {

      }

      for {
        neighbor <- getNeighbors(map, current.position)
        if !closedSet.contains(neighbor)
      } {
        val newCost = costSoFar.getOrElse(current.position, 0L) + costFromCurrentToNeighbor(map, neighbor)
        if (costSoFar.get(neighbor).map(newCost < _).getOrElse(true)) {
          costSoFar.update(neighbor, newCost)
          val h = heuristic.estimateDistance(current.position, neighbor)
          openSet.add(WeightedNode(neighbor, h))
          cameFrom.update(neighbor, Some(current.position))
        }
      }
    }

    None
  }

  private def getNeighbors(map: Matrix2D, position: Position): List[Position] = {
    for {
      dx <- List(-1, 1)
      dy <- List(-1, 1)
      if map.isDefinedAt(position.x + dx)
      if map(position.x + dx).isDefinedAt(position.y + dy)
    } yield Position(position.x + dx, position.y + dy)
  }

  private def costFromCurrentToNeighbor(map: Matrix2D, neighbor: Position): Long = {
    if (map.isDefinedAt(neighbor.x) && map(neighbor.x).isDefinedAt(neighbor.y)) {
      val node = map(neighbor.x)(neighbor.y)
      node.state match {
        case NodeState.IMPASS => 100
        case NodeState.NORMAL => 1
        case NodeState.SLOW => 50
        case _ => 1000
      }
    } else {
      1
    }
  }

  private case class WeightedNode(position: Position, fScore: Long) extends Comparable[WeightedNode] {
    override def compareTo(o: WeightedNode): Int = this.fScore.compareTo(o.fScore)
  }
}
