package BeerCrush

object BeerCrush {
	val ISO8601DateFormat="yyyy-MM-dd'T'HH:mm:ssZ"
}

abstract class PersistentObject(val id: Option[Id]) {
	val pageURL: String
	def save: Unit
}

object PersistentObject {
	def fileLocationFromId(id: Id) = {
		"/Users/troy/beerdata/beer/" + id.toString + ".xml"
	}
}
	
class Id(val id: Option[String]) {
	override def toString = id.getOrElse("")
}
object Id {
	implicit def id2string(id: Id):String = id.id.get
	implicit def string2id(id: String) = new Id(Some(id))
	implicit def oid2string(id: Option[Id]): String = id.toString
}
	
case class BreweryId(breweryId: String) extends Id(Some(breweryId)) {
	// TODO: verify the id looks like a brewery id
	lazy val pageURL = { "/" + id }
}
object BreweryId {
	implicit def string2id(s: String): BreweryId = { new BreweryId(s) }
	// implicit def string2oid(id: String): Option
}

case class BeerId(beerId: String) extends Id(Some(beerId)) {
	// TODO: verify the id looks like a beer id
	def breweryId: BreweryId = beerId.split('/').first
}
object BeerId {
	implicit def string2id(s: String): BeerId = { new BeerId(s) }
	implicit def string2oid(id: String): Option[BeerId] = Some(new BeerId(id))
}

