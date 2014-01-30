package ayai.components

import com.artemis.Component

import net.liftweb.json.JsonDSL._
import net.liftweb.json._


case class Weapon(
    name: String,
    value: Int,
    weight: Double,
    range: Int,
    damage: Int,
    damageType: String)
  extends Item(name, itemType = "weapon", value, weight) {

  override def asJson : JObject = {
      ("name" -> name) ~
      ("value" -> value) ~
      ("weight" -> weight)
  }

  override def toString: String = {
    "{\"name\": " + name + "}"
  }

  
}