package ayai.gamestate

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.collection.mutable.HashMap
import ayai.components._
import akka.actor.Status.{Success, Failure}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

case class AddNPC(id: String, npc: NPCValues)
case class RemoveNPC(id: String)
case class GetNPC(id: String)
case class NPCValues(
      id: Int,
      name: String,
      faction: String,
      roomId: Int,
      equipment: Equipment,
      level: Int,
      experience: Long,
      maximumHealth: Int,
      maximumMana: Int) {
	
	def asJson(): JObject = {
		("id" -> id) ~
		("name" -> name) ~
		("faction" -> faction) ~
		("roomId" -> roomId) ~
		("equipment" -> equipment.asJson) ~
		("level" -> level) ~
		("experience" -> experience) ~
		("maximumHealth" -> maximumHealth) ~
		("maximumMana" -> maximumMana)
	}
}
class NPCMap() extends Actor {
	val npcMap: HashMap[String, NPCValues] = HashMap[String, NPCValues]()

	def addNPC(id: String, npc: NPCValues) = {
		npcMap(id) = npc
	}

	def removeNPC(id: String) = {
		npcMap -= id
	}

	def getNPC(id: String) = {
		sender ! npcMap(id)
	}

	def outputJson() = {
		
		sender ! compact(render(("npcs" -> (npcMap.map{case (key, value) => (value.asJson)}))))
	}
	def receive = {
		case AddNPC(id: String, npc: NPCValues) => addNPC(id, npc)
		case RemoveNPC(id: String) => removeNPC(id)
		case GetNPC(id: String) => getNPC(id)
		case OutputJson() => outputJson
		case _ =>
			sender ! Failure
	}
}