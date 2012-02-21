package BeerCrush

abstract class PersistentObject(val id: Id) {
	val pageURL: String
	def save: Unit
}

object PersistentObject {
	def fileLocationFromId(id: Id) = {
		"/Users/troy/beerdata/beer/" + id.toString + ".xml"
	}
}
	
class Id(val id: String) {
	override def toString = id
}
object Id {
	implicit def id2string(id: Id):String = id.id
	implicit def string2id(id: String) = new Id(id)
}
	
case class BreweryId(breweryId: String) extends Id(breweryId) {
	// TODO: verify the id looks like a brewery id
	lazy val pageURL = { "/" + id }
}
object BreweryId {
	implicit def string2id(s: String): BreweryId = { new BreweryId(s) }
}

case class BeerId(beerId: String) extends Id(beerId) {
	// TODO: verify the id looks like a beer id
}
object BeerId {
	implicit def string2id(s: String): BeerId = { new BeerId(s) }
}

