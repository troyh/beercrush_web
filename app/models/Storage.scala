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
				BeerCrush.fileLocation(item.id.get).split("/").dropRight(1).mkString("/"),
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

		/* Make the necessary directories to store the document */
		BeerCrush.mkpath(BeerCrush.fileLocation(itemToSave.id.get))

		// TODO: original file may not exist, handle that.
		val oldXML=scala.xml.XML.load(BeerCrush.fileLocation(itemToSave.id.get))

		def removeEmptyElements(ns: NodeSeq):NodeSeq = for (n <- ns) yield n match {
			case e: Elem if (e.attributes.length == 0 && e.child.length == 0) => Text("") /* Output nothing */
			case e: Elem => e.copy(child=removeEmptyElements(e.child))
			case other => other
		}
		
		val newXML=removeEmptyElements(itemToSave.transform(oldXML))

		val pp=new scala.xml.PrettyPrinter(80,2)
		Path(BeerCrush.fileLocation(itemToSave.id.get)).write(
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			pp.formatNodes(newXML) +
			"\n")(Codec.UTF8)
		
		itemToSave.id.get
	}
}

