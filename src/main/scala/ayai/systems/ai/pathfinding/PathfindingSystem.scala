package ayai.systems.ai.pathfinding

import akka.actor.ActorSystem
import ayai.components.{Vision, Position}
import ayai.components.pathfinding._
import ayai.factories.GraphFactory
import ayai.gamestate.RoomWorld
import crane.{Entity, EntityProcessingSystem}

object PathfindingSystem {
  val COMPONENT_INCLUDES: List[AnyRef] = List(
    // TODO: currently Crane doesn't allow for specifying component types by root class (superclass)
    // It would be nice to change this so we could query for all entities which have components inheriting from "Pathfinder"
    classOf[AStarPathfinder],
    classOf[Position]
  )

  def apply(actorySystem: ActorSystem): PathfindingSystem = new PathfindingSystem(actorySystem)
}

class PathfindingSystem(actorSystem: ActorSystem) extends EntityProcessingSystem(include = PathfindingSystem.COMPONENT_INCLUDES) {

  def processEntity(e: Entity, delta: Int): Unit = {
    val roomWorld = world.asInstanceOf[RoomWorld]
    val map = GraphFactory.generateGraph(roomWorld)

    for {
      pathfinding <- e.getComponent(classOf[AStarPathfinder]).toList.map(_.asInstanceOf[AStarPathfinder])
      position <- e.getComponent(classOf[Position]).toList.map(_.asInstanceOf[Position])
      vision <- e.getComponent(classOf[Vision]).toList.map(_.asInstanceOf[Vision])
    } {

      if (vision.entitiesInSight.isEmpty) {
        return
      }

      val targetPosition = vision.entitiesInSight.head.getComponent(classOf[Position]).map(_.asInstanceOf[Position])

      if (targetPosition.isEmpty) {
        return
      }

      // TODO: replace with real target
      val graphView = new NodeMatrixGraphView(map, pathfinding.movementStyle)
      val conversionRatio: Float = roomWorld.tileMap.tileSize
      val nmStartPosition = NodeMatrixPosition.fromPosition(position, conversionRatio)
      val nmTargetPosition = NodeMatrixPosition.fromPosition(targetPosition.get, conversionRatio)
      pathfinding.findPath(graphView, nmStartPosition, nmTargetPosition) match {
        case Some(path) => {
          if (path.isEmpty) {
            // We are at the goal
          } else {
            // TODO: this is extremely inefficient but I'm putting it in for testing purposes
            val newPath = path.map(_.toPosition(conversionRatio)).toList
            val existingPath = pathfinding.currentPath.toList

            if (!pathsAreEqual(newPath, existingPath)) {
              pathfinding.pathHasChanged = true
              pathfinding.currentPath.clear()
              pathfinding.currentPath ++= path.map(_.toPosition(conversionRatio)).toList
            } else {
              pathfinding.pathHasChanged = false
            }

            // Change velocity, position, whatever
            // This part will probably call .toPosition(conversionRatio) on a NodeMatrixPosition
          }
        }
        case None => {
          // There is no path from start to end
        }
      }
    }
  }

  private def pathsAreEqual(path1: List[Position], path2: List[Position]): Boolean = {
    if (path1.length != path2.length) {
      return false
    }

    path1.zip(path2).forall {
      case (pos1, pos2) => pos1.x == pos2.x && pos1.y == pos2.y
    }
  }
}
