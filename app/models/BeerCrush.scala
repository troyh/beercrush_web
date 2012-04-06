package BeerCrush

import play.api.data._
import play.api.data.validation.{Constraint, Constraints, Valid, Invalid, ValidationError}
import models._
import scala.annotation._
import scala.xml.{NodeSeq, Elem, Text}
import scalax.file.Path
import scalax.io._
import java.util.UUID

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

	def newUniqueFilename(directory: String,basisForId: String): String = {

		def fileexists(filename: String) = new java.io.File(filename).exists()
			
		@tailrec
		def tryFilePath(path: String, filename: String, suffixes: Seq[Seq[_]]): String = {
			if (!fileexists(path + "/" + filename + ".xml"))
				return filename
			if (suffixes.length == 0)
				return filename + "-" + UUID.randomUUID() // Give up on creating a nice Id, and just use a UUID at the end
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

	def directoryPath(id: UniqueId[_]) = id match {
		case _: Beer.Id    => BeerCrush.datadir + "/beer/"
		case _: Brewery.Id => BeerCrush.datadir + "/brewery/"
		case _: User.Id    => BeerCrush.datadir + "/user/"
		case _: ReviewId  => BeerCrush.datadir + "/beer/"
		// case _: BeerStyle.Id   => BeerCrush.datadir + "/beerstyles.xml"
	}

	def fileLocation(id: UniqueId[_]) = id match {
		case _: Beer.Id    => directoryPath(id) + id.toString + ".xml"
		case _: Brewery.Id => directoryPath(id) + id.toString + ".xml"
		case _: User.Id    => directoryPath(id) + id.toString + ".xml"
		case _: ReviewId  => directoryPath(id) + id.toString + ".xml" // TODO: Handle Beer Reviews separately from generic ReviewIds
		// case _: BeerStyle.Id   => BeerCrush.datadir + "/beerstyles.xml"
	}

	// def save(item: XmlFormat): Unit = {
	def save(item: { def toXML: NodeSeq; def id: UniqueId[_]; def transform(nodes: NodeSeq): NodeSeq } ): Unit = {
		assert(item.id.isDefined)
		
		/* Make the necessary directories to store the document */
		BeerCrush.mkpath(fileLocation(item.id))

		// TODO: original file may not exist, handle that.
		val oldXML=scala.xml.XML.load(fileLocation(item.id))

		def removeEmptyElements(ns: NodeSeq):NodeSeq = for (n <- ns) yield n match {
			case e: Elem if (e.attributes.length == 0 && e.child.length == 0) => Text("") /* Output nothing */
			case e: Elem => e.copy(child=removeEmptyElements(e.child))
			case other => other
		}
		
		val newXML=removeEmptyElements(item.transform(oldXML))

		val pp=new scala.xml.PrettyPrinter(80,2)
		Path(fileLocation(item.id)).write(
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			pp.formatNodes(newXML) +
			"\n")(Codec.UTF8)
	}
	
}

class UniqueId[+T](val id: T) {
	override def toString = id.toString
	val isDefined = true
	
	// def fileLocation: String
	
}

object UndefinedId extends UniqueId[Null](null) {
	override val isDefined = false
	// val fileLocation = ""
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
