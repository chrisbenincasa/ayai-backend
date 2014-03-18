package ayai.components

import crane.Component
import net.liftweb.json.Serialization.{read, write}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

case class SpriteImage(image: String) extends Component {
	def asJson: JObject = {
		("spriteimage" -> image)
	}
}