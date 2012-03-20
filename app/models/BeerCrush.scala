package BeerCrush

import play.api.data._
import play.api.data.validation.{Constraint, Constraints, Valid, Invalid, ValidationError}

object BeerCrush {
	val ISO8601DateFormat="yyyy-MM-dd'T'HH:mm:ssZ"
	val SolrDateFormat="yyyy-MM-dd'T'HH:mm:ss'Z'"
	final val xmlNamespace="beercrush"
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
	implicit def string2oid(id: String): Option[BreweryId] = Some(new BreweryId(id))
}

case class BeerId(beerId: String) extends Id(Some(beerId)) {
	// TODO: verify the id looks like a beer id
	def breweryId: BreweryId = beerId.split('/').head
}
object BeerId {
	implicit def string2id(s: String): BeerId = { new BeerId(s) }
	implicit def string2oid(id: String): Option[BeerId] = Some(new BeerId(id))
}

object FormConstraints {
	def minLength(length: Int): Constraint[Option[String]] = Constraint[Option[String]]("constraint.minLength", length) { o =>
		if (o.size >= length) Valid else Invalid(ValidationError("error.minLength", length))
	}
    def minString(minValue: Int): Constraint[String] = Constraint[String]("constraint.min", minValue) { o =>
		if (o.toInt >= minValue) Valid else Invalid(ValidationError("error.min", minValue))
    }
    def min(minValue: Double): Constraint[Double] = Constraint[Double]("constraint.min", minValue) { o =>
		if (o >= minValue) Valid else Invalid(ValidationError("error.min", minValue))
    }
    def max(maxValue: Double): Constraint[Double] = Constraint[Double]("constraint.max", maxValue) { o =>
		if (o <= maxValue) Valid else Invalid(ValidationError("error.max", maxValue))
    }
    def minOptional(minValue: Int): Constraint[Option[String]] = Constraint[Option[String]]("constraint.min", minValue) { o =>
		o match {
			case None => Valid
			case Some(s) => if (s.toInt >= minValue) Valid else Invalid(ValidationError("error.min", minValue))
		}
    }
    def maxString(maxValue: Int): Constraint[String] = Constraint[String]("constraint.max", maxValue) { o =>
		if (o.toInt <= maxValue) Valid else Invalid(ValidationError("error.max", maxValue))
    }
    def maxOptional(maxValue: Int): Constraint[Option[String]] = Constraint[Option[String]]("constraint.max", maxValue) { o =>
		o match {
			case None => Valid
			case Some(s) => if (s.toInt <= maxValue) Valid else Invalid(ValidationError("error.max", maxValue))
		}
    }
    def pattern(regex: scala.util.matching.Regex, name: String = "constraint.pattern", error: String = "error.pattern"): Constraint[String] = Constraint[String](name, regex) { o =>
		regex.unapplySeq(o).map(_ => Valid).getOrElse(Invalid(ValidationError(error, regex)))
    }
    def patternOptional(regex: scala.util.matching.Regex, name: String = "constraint.pattern", error: String = "error.pattern"): Constraint[Option[String]] = Constraint[Option[String]](name, regex) { o =>
		o match {
			case None => Valid
			case Some(s) => regex.unapplySeq(o).map(_ => Valid).getOrElse(Invalid(ValidationError(error, regex)))
		}
    }
}
