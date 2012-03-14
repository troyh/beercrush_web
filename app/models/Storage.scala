package models

import BeerCrush._
import scala.annotation.tailrec
import scalax.file.Path
import scalax.io._
import scala.xml._
import play.api._

object Storage {
	
	trait Saveable extends XmlFormat {
		def id: Option[Id]
		def ctime: Option[java.util.Date]
		def descriptiveNameForId: String
		def dupe(id:Id,ctime:java.util.Date): Saveable
		def transform(nodes: NodeSeq, elementNames: Seq[String] = Seq(), xpath: String = ""): NodeSeq

		/**
		* Used for subclasses to implement transform() easily
		*/
		protected def transform_base(nodes: NodeSeq, elementNames: Seq[String], foo: (Node, NodeSeq) => Node) = {
			(for (node <- nodes ++ elementNames.filter(e => !nodes.exists(n => e==n.label)).map { e =>
				/* 
					Add empty elements for all the ones in the list above that don't already exist
				*/
				Elem(null,e,Null,xml.TopScope) 
			}) yield node match { 
				/*
					Replace elements with data from this object, keeping elements we don't care about intact
				*/ 
				case Elem(prefix, label, attribs, scope, children @ _*) => foo(node,children)
				case other => other
			}).filter(_.child.length > 0) // Strip out any empty elements
		}

	}
	
	val datadir="/Users/troy/beerdata"
	lazy val datadir_parts=datadir.split("/")
	
	def fileLocation(id: Id) = id match {
		case _: BeerId    => datadir + "/beer/" + id.toString + ".xml"
		case _: BreweryId => datadir + "/brewery/" + id.toString + ".xml"
		case _: UserId    => datadir + "/user/" + id.toString + ".xml"
		case _: ReviewId  => datadir + "/beer/" + id.toString + ".xml" // TODO: Handle Beer Reviews separately from generic ReviewIds
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
		
		val oldXML=scala.xml.XML.load(fileLocation(itemToSave.id.get))
		val newXML=itemToSave.transform(oldXML)

		val pp=new scala.xml.PrettyPrinter(80,2)
		Path(fileLocation(itemToSave.id.get)).write(
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
			pp.formatNodes(newXML) +
			"\n")(Codec.UTF8)
		
		itemToSave.id.get
	}
}

