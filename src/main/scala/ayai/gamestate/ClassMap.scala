package ayai.gamestate

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.collection.mutable.HashMap
import ayai.components._
import akka.actor.Status.{Success, Failure}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import ayai.factories.ClassFactory
import scala.collection.mutable.ArrayBuffer

case class AddClass(id: String, thisClass: ClassValues)
case class GetClass(id: String)
case class RemoveClass(id: String)
case class OutputJson()
case class GetClassByName(name: String)
case class ClassValues(
	  id: Int, 	
      name: String,
      description: String,
      baseHealth: Int,
      baseMana: Int,
      baseStats: Stats) {

	def asJson(): JObject = {
		("id" -> id) ~
		("name" -> name) ~
		("description" -> description) ~
		("baseHealth" -> baseHealth) ~
		("baseMana" -> baseMana) ~
		("baseStats" -> baseStats.asJson)
	}
}
class ClassMap() extends Actor {
	val classMap: HashMap[String, ClassValues] = HashMap[String, ClassValues]()

	def receive = {
		case AddClass(id: String, thisClass: ClassValues) =>
		  classMap(id) = thisClass
		case GetClass(id: String) =>
		  val classValues = classMap(id)
		  if(classValues != null) {
		  	sender ! classValues
		  } else {
		  	// create default
		  	val stats: Stats = new Stats()
		  	stats.addStat(new Stat("strength", 15, 5))
		  	stats.addStat(new Stat("agility", 20, 5))
		  	stats.addStat(new Stat("intelligence", 5, 5))
		  	sender ! new ClassValues(1, "warrior", "Beats up with pure strength", 100, 20, stats)
		  }
		case GetClassByName(name: String) =>
		  val classValues = classMap.find{ case(key, value) => if(value.name.toLowerCase == name.toLowerCase) { true } else {false } }
		  classValues match {
		  	case Some((key:String, value: ClassValues)) => sender ! value
		  	case _ =>  		  	
			  	// create default
			  	val stats: Stats = new Stats()
			  	stats.addStat(new Stat("strength", 15, 5))
			  	stats.addStat(new Stat("agility", 20, 5))
			  	stats.addStat(new Stat("intelligence", 5, 5))
			  	sender ! new ClassValues(1, "warrior", "Beats up with pure strength", 100, 20, stats)
		  }
		  
		case RemoveClass(name: String) =>
		  classMap -= name
		case OutputJson() =>
			sender ! compact(render((classMap.map{case (key, value) => value.asJson})))
		case _ => println("No Command for Classes")
	 		sender ! Failure
	}
}
