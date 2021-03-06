package ayai.components

import crane.{Component, Entity}
import ayai.statuseffects._ 
import scala.collection.mutable.{ArrayBuffer, HashMap}

case class ItemUse(val initiator: Entity, val item: Item, var target: Entity = null) extends Component {
	if(target == null) {
		target = initiator
	}
	def getItemEffects(): ArrayBuffer[Effect] = {
		item.effects
	}
}