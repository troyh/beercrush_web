package BeerCrush

import play.api.data._
import play.api.data.validation.{Constraint, Constraints, Valid, Invalid, ValidationError}
import models._
import scala.annotation._
import scala.xml.{NodeSeq, Elem, Text}
import scalax.file.Path
import scalax.io._

object BeerCrush {
	val ISO8601DateFormat="yyyy-MM-dd'T'HH:mm:ssZ"
	val SolrDateFormat="yyyy-MM-dd'T'HH:mm:ss'Z'"
	final val xmlNamespace=new scala.xml.NamespaceBinding("bc","http://beercrush.org",null)

	val datadir="/Users/troy/beerdata/"
	lazy val datadir_parts=datadir.split("/")

	def mkpath(path: String) = path.split("/").drop(BeerCrush.datadir_parts.length).foldLeft(BeerCrush.datadir){ (path,item) => 
		val f=new java.io.File(path)
		f.mkdir()
		path + "/" + item
	}

	def newUniqueFilename(directory: String,basisForId: String): Option[String] = {

		def fileexists(filename: String) = new java.io.File(filename).exists()
			
		@tailrec
		def tryFilePath(path: String, filename: String, suffixes: Seq[Seq[_]]): Option[String] = {
			if (!fileexists(path + "/" + filename + ".xml"))
				return Some(filename)
			if (suffixes.length == 0)
				return None
			if (suffixes.head.length == 1) {
				suffixes.head.head match {
					case s: String => tryFilePath(path,filename + "-" + s, suffixes.tail)
					case n: Int    => tryFilePath(path,filename + "-" + n.toString, suffixes.tail)
				}
			}
			else
				tryFilePath(path,filename,Seq(suffixes.head.tail) ++ suffixes.tail)
		}

		tryFilePath(
			directory,
			"[^a-zA-Z0-9]+".r.replaceAllIn(basisForId.replace("'",""),"-"),
			Seq(
				Seq("2012"),
				Range(2,20)
			)
		)
	}

	// def fileLocation(id: Id) = id match {
	// 	case _: BeerId    => id.directoryPath() + id.toString + ".xml"
	// 	case _: BreweryId => id.directoryPath() + id.toString + ".xml"
	// 	case _: UserId    => id.directoryPath() + id.toString + ".xml"
	// 	case _: ReviewId  => id.directoryPath() + id.toString + ".xml" // TODO: Handle Beer Reviews separately from generic ReviewIds
	// 	case _: StyleId   => BeerCrush.datadir + "/beerstyles.xml"
	// }

	// def save(item: XmlFormat): Unit = {
	def save(item: { def toXML: NodeSeq; def id: Id; def transform(nodes: NodeSeq): NodeSeq } ): Unit = {
		
		/* Make the necessary directories to store the document */
		BeerCrush.mkpath(item.id.fileLocation)

		// TODO: original file may not exist, handle that.
		val oldXML=scala.xml.XML.load(item.id.fileLocation)

		def removeEmptyElements(ns: NodeSeq):NodeSeq = for (n <- ns) yield n match {
			case e: Elem if (e.attributes.length == 0 && e.child.length == 0) => Text("") /* Output nothing */
			case e: Elem => e.copy(child=removeEmptyElements(e.child))
			case other => other
		}
		
		val newXML=removeEmptyElements(item.transform(oldXML))

		val pp=new scala.xml.PrettyPrinter(80,2)
		Path(item.id.fileLocation).write(
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			pp.formatNodes(newXML) +
			"\n")(Codec.UTF8)
	}
	
}

abstract class Id(val id: Option[String]) {
	override def toString = id.getOrElse("")
	
	def fileLocation: String
	// def directoryPath = this match {
	// 	case _: BeerId    => BeerCrush.datadir + "/beer/" +
	// 	case _: BreweryId => BeerCrush.datadir + "/brewery/"
	// 	case _: UserId    => BeerCrush.datadir + "/user/"
	// 	case _: ReviewId  => BeerCrush.datadir + "/beer/" +
	// 	// case _: StyleId   => BeerCrush.datadir + "/beerstyles.xml"
	// }

}
object Id {
	implicit def id2string(id: Id):String = id.id.get
	// implicit def string2id(id: String) = new Id(Some(id))
	// implicit def oid2string(id: Option[Id]): String = id.toString
}
	
case class BreweryId(breweryId: String) extends Id(Some(breweryId)) {
	// TODO: verify the id looks like a brewery id
	lazy val pageURL = { "/" + id }
	def fileLocation = BeerCrush.datadir + "/brewery/" + breweryId.toString
}
object BreweryId {
	implicit def string2id(s: String): BreweryId = { new BreweryId(s) }
	implicit def string2oid(id: String): Option[BreweryId] = Some(new BreweryId(id))

	def newUniqueId(breweryName: String): Option[String] = 
		BeerCrush.newUniqueFilename(BeerCrush.datadir + "/brewery/",breweryName)
}

case class BeerId(beerId: String) extends Id(Some(beerId)) {
	// TODO: verify the id looks like a beer id
	def breweryId: BreweryId = beerId.split('/').head
	def fileLocation = BeerCrush.datadir + "/beer/" + id.get + ".xml"
}
object BeerId {
	implicit def string2id(s: String): BeerId = { new BeerId(s) }
	implicit def string2oid(id: String): Option[BeerId] = Some(new BeerId(id))

	def newUniqueId(breweryId: BreweryId, beerName: String): Option[String] = 
		BeerCrush.newUniqueFilename(breweryId.fileLocation.stripSuffix(".xml"),beerName)
	
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
