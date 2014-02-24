package ayai.systems

/** 
 * ayai.system.CollisionSystem
 */

/** Ayai Imports **/
import ayai.actions._
import ayai.components._
import ayai.collisions._

/** Crane Imports **/
import crane.{Entity, System, World}

/** External Imports **/
import scala.math.abs
import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._

import org.slf4j.{Logger, LoggerFactory}

class CollisionSystem() extends System {
  private val log = LoggerFactory.getLogger(getClass)

  def valueInRange(value: Int, min: Int, max: Int): Boolean = {
    return (value >= min) && (value <= max)
  }


  //Eventually create a damage system to calculate this based on the users
  //equipped item
  def handleAttackDamage(damage : Int, attackee: Health) {
    val currentHealth = attackee.getCurrentHealth
    println("Damage Detected!")
    attackee.setCurrentHealth(currentHealth - damage)
  }

  def getWeaponStat(entity : Entity) : Int = {
    var playerBase : Int = 0
    var weaponValue : Int = 0
    entity.getComponent(classOf[Stats]) match {
      case Some(stats : Stats) => 
        for(stat <- stats.stats) {
          if(stat.attributeType == "strength"){
            playerBase += stat.magnitude
          }
        }
      case _ =>
    }
    entity.getComponent(classOf[Equipment]) match {
      case Some(equipment : Equipment) =>
        if(equipment.weapon1 != null) {
          weaponValue += equipment.weapon1.damage
        } 
        if(equipment.weapon2 != null) {
          weaponValue += equipment.weapon2.damage
        }
      case _ => 
    }
    return weaponValue + playerBase
  }

  def getArmorStat(entity : Entity) : Int = {
    var playerBase : Int = 0
    var armorValue : Int = 0
    entity.getComponent(classOf[Stats]) match {
      case Some(stats : Stats) => 
        for(stat <- stats.stats) {
          if(stat.attributeType == "defense"){
            playerBase += stat.magnitude
          }
        }
      case _ =>
    }
    return armorValue + playerBase

  }

  def handleAttack(entityA: Entity, entityB: Entity):Boolean = {
    (entityA.getComponent(classOf[Attack]),
      entityB.getComponent(classOf[Attack]),
      entityA.getComponent(classOf[Health]),
      entityB.getComponent(classOf[Health])) match {
      case(Some(attackComponentA : Attack), None, None, Some(healthComponentB : Health)) =>
          //calculate the attack
          var damage : Int = getWeaponStat(entityA.initiator)
          damage -= getArmorStat(entityB)
          //remove the attack component of entity A
          var damageDone = handleAttackDamage(damage, healthComponentB)
          val initiatorId = attackComponent.initiator.getComponent(classOf[Character]) match {
            case Some(character : Character) =>
              character.id 
          }
          val victim = entityB.getComponent(classOf[Character]) match {
            case Some(character : Character) => character.id
          }
          
          world.getSystem(classOf[NetworkingSystem]) match {
            case Some(network : NetworkingSystem) =>
              val att = ("attack" -> 
                ("damage" -> damage) ~
                ("initiator" -> initiatorId) ~
                ("victim" -> victim)) 
              networkSystem.broadcastMessage(compact(render(att)))
          }
          
          entityA.kill()
          entityA.components += new Dead()
          true
      case (None, Some(attackComponentB : Attack), Some(healthComponentA : Health), None) =>
          getWeaponStat(entityB)
          //remove the attack component of entityB
          handleAttackDamage(attackComponentB, healthComponentA)
          entityB.kill()
          entityB.components += new Dead()
          true
      case _ => false
      }
  }

  def handleCollision(entityA: Entity, entityB: Entity) {
      (entityA.getComponent(classOf[Position]),
        entityA.getComponent(classOf[Bounds]),
        entityB.getComponent(classOf[Position]),
        entityB.getComponent(classOf[Bounds])) match {
        case(Some(positionA : Position), Some(boundsA : Bounds), Some(positionB : Position), Some(boundsB : Bounds)) =>
          // Remember to end a line with an operator of some sort (., +, &&, ||) if you need 
          // to not fall afoul of the automatic end of statement guesser
          //are the two characters within the same x position (is A between B's leftside and width length) 
          val xOverlap: Boolean = valueInRange(positionA.x, positionB.x, positionB.x + boundsB.width) ||
                                  valueInRange(positionB.x, positionA.x, positionA.x + boundsA.width)

          //are the two characters within the same height area.
          val yOverlap: Boolean = valueInRange(positionA.y, positionB.y, positionB.y + boundsB.height) ||
                                  valueInRange(positionB.y, positionA.y, positionA.y + boundsA.height)

          if(xOverlap && yOverlap) {
            if (handleAttack(entityA, entityB)) {
              return;
            }
            //check to see if they are movable
            if(abs(positionA.y - positionB.y) < abs(positionA.x - positionB.x)) {
              if(positionA.x < positionB.x) {
                LeftDirection.process(entityA)
                RightDirection.process(entityB)
              } else {
                RightDirection.process(entityA)
                LeftDirection.process(entityB)
              }
            } else {
              if(positionA.y < positionB.y) {
                UpDirection.process(entityA)
                DownDirection.process(entityB)
              } else {
                DownDirection.process(entityA)
                UpDirection.process(entityB)
              }
            }
          }
        case _ => return
      }
  }


  override def process(delta : Int) {
    //for each room get entities
    val exclusion = List(classOf[Respawn], classOf[Transport], classOf[Dead])
    val rooms : ArrayBuffer[Entity] = world.groups("ROOMS")
    for(e <- rooms) {
      // This needs to warn us better
      val tileMap = (e.getComponent(classOf[TileMap]): @unchecked) match {
        case(Some(tilemap : TileMap)) => 
          tilemap
      }
      val room = (e.getComponent(classOf[Room])) match {
        case (Some(r : Room)) => r
        case _ =>
          log.warn("0b38a4a: getComponent failed to return anything")
          new Room(-1)
      }
      
      var quadTree : QuadTree = new QuadTree(0, new Rectangle(0,0,tileMap.getMaximumWidth, tileMap.getMaximumHeight))
      var entities = world.groups("ROOM"+room.id).toList
      entities = excludeList(entities, exclusion)
      for(entity <- entities) {
        quadTree.insert(entity)
      }

      val quads = quadTree.quadrants
      for(quad <- quads) {
        while(quad.length > 1) {
          for(against <- quad.tail) {
            if(!hasExclusion(quad.head, exclusion)) {
              handleCollision(quad.head, against)
            }
          }
          quad.remove(0)
        }
      }
    }
  }

  def excludeList[T <: AnyRef](entities : List[Entity],exclusionList : List[T] ) : List[Entity] = {
    entities.filter{ entity =>
      val componentEntityTypes : Set[Object] = entity.components.map(c=>c.getClass).toSet 
      (exclusionList.toSet intersect componentEntityTypes).isEmpty 
    }.toList

  }

  def hasExclusion[T <: AnyRef](entity : Entity, exclusionList : List[T]) : Boolean = {
      val componentEntityTypes : Set[Object] = entity.components.map(c=>c.getClass).toSet 
      !(exclusionList.toSet intersect componentEntityTypes).isEmpty
    }
}
