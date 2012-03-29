package models

import BeerCrush._
import SuperNode._
import scala.annotation.tailrec
import scalax.file.Path
import scalax.io._
import scala.xml._
import play.api._
import scala.collection.immutable._
import scala.collection.immutable.Range._

object Storage {
	
	trait Saveable extends XmlFormat {
		def id: Option[Id]
		def ctime: Option[java.util.Date]
		def descriptiveNameForId: String
		def dupe(id:Id,ctime:java.util.Date): Saveable
	}
	
	val datadir="/Users/troy/beerdata"
	lazy val datadir_parts=datadir.split("/")
	
	def fileLocation(id: Id) = id match {
		case _: BeerId    => datadir + "/beer/" + id.toString + ".xml"
		case _: BreweryId => datadir + "/brewery/" + id.toString + ".xml"
		case _: UserId    => datadir + "/user/" + id.toString + ".xml"
		case _: ReviewId  => datadir + "/beer/" + id.toString + ".xml" // TODO: Handle Beer Reviews separately from generic ReviewIds
		case _: StyleId   => datadir + "/beerstyles.xml"
	}
	
	def transform(item: Saveable, nodes: NodeSeq): NodeSeq = item match {
		case style: BeerStyle => for (node <- nodes) yield node match {
			case s @ <style>{_*}</style> => { 
				val updates=Attribute("","id",style.id.get.toString, Null) ++
					Attribute("","name",  style.name, Null)
				val rangeUpdates=(
					(style.abv,"ABV") ::
					(style.ibu,"IBU") :: 
					(style.og,"OG") :: 
					(style.fg,"FG") :: 
					(style.srm,"SRM") :: 
				 Nil).foldLeft(updates) { (r, pair) => r ++ (pair._1 match {
						case Some(v) => Attribute("",pair._2+"lo", v.start.toString, Attribute("",pair._2+"hi", v.end.toString,Null))
						case None => Null
				})}

				rangeUpdates.foldLeft(s.asInstanceOf[Elem])( _ % _ )
			}
			case other => other
		}
	}

	lazy private val beerStylesXML=scala.xml.XML.loadFile(fileLocation(StyleId(Some(""))))

	/**
	  * Retrieves a Saveable object from storage.
	  *
	  * @param id The Id of the object
	  */
	def load(id: Id): Option[Saveable] = id match {
		case styleId: StyleId if (styleId.styleId.get.isEmpty) => Some(BeerStyle(
			styleId=""
			,name="Beer"
			,substyles  =beerStylesXML.child.map(_.attribute("id").map(n => StyleId(Some(n.text))))
		))
		case styleId: StyleId => {
			beerStylesXML \\ "style" find { _.attribute("id").getOrElse("") .toString == id.toString } match { 
				case None => None
				case Some(node) => node.attribute("id") match {
					case None => None
					case Some(id) => Some(BeerStyle(
						styleId=id.text
						,name=node.attribute("name").get.text
						,abv= (node.attribute("ABVlo"), node.attribute("ABVhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble, hi.head.text.toDouble, 0.01))
							case _ => None
						}
						,ibu=(node.attribute("IBUlo"), node.attribute("IBUhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Int.inclusive(lo.head.text.toInt,hi.head.text.toInt,1))
							case _ => None
						}
						,og= (node.attribute("OGlo"),  node.attribute("OGhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble,hi.head.text.toDouble, 0.001))
							case _ => None
						}
						,fg= (node.attribute("FGlo"),  node.attribute("FGhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble, hi.head.text.toDouble, 0.001))
							case _ => None
						}
						,srm= (node.attribute("SRMlo"), node.attribute("SRMhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Int.inclusive(lo.head.text.toInt, hi.head.text.toInt, 1))
							case _ => None
						}
						,origin=node.attribute("origin").map(_.text)
						,superstyles=node.getAncestors(beerStylesXML).map(_.attribute("id").map(n => StyleId(Some(n.text))))
						,substyles  =node.child.map(_.attribute("id").map(n => StyleId(Some(n.text))))
					))
				}
			}
		}
	}
	
	def save[T <: Saveable](item: T): Id = {
		
		def makeNewId(item: Saveable): Option[String] = {

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
				fileLocation(item.id.get).split("/").dropRight(1).mkString("/"),
				"[^a-zA-Z0-9]+".r.replaceAllIn(item.descriptiveNameForId.replace("'",""),"-"),
				Seq(
					Seq("2012"),
					Range(2,20)
				)
			)
		}

		val itemToSave=item.dupe(
			id=item.id.getOrElse(makeNewId(item).get),
			ctime=item.ctime.getOrElse(new java.util.Date())
		)

		/* Make the necessary directories to store the review */
		fileLocation(itemToSave.id.get).split("/").drop(datadir_parts.length).foldLeft(datadir){ (path,item) => 
			val f=new java.io.File(path)
			f.mkdir()
			path + "/" + item
		}

		// TODO: original file may not exist, handle that.
		val oldXML=scala.xml.XML.load(fileLocation(itemToSave.id.get))

		def removeEmptyElements(ns: NodeSeq):NodeSeq = for (n <- ns) yield n match {
			case e: Elem if (e.attributes.length == 0 && e.child.length == 0) => Text("") /* Output nothing */
			case e: Elem => e.copy(child=removeEmptyElements(e.child))
			case other => other
		}
		
		val newXML=removeEmptyElements(itemToSave.transform(oldXML))

		val pp=new scala.xml.PrettyPrinter(80,2)
		Path(fileLocation(itemToSave.id.get)).write(
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			pp.formatNodes(newXML) +
			"\n")(Codec.UTF8)
		
		itemToSave.id.get
	}
}

