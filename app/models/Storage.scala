package models

import BeerCrush._

object Storage {
	
	trait Saveable extends XmlFormat {
		def id: Option[Id]
		def ctime: Option[java.util.Date]
		def dupe(id:Id,ctime:java.util.Date): Saveable
	}
	
	val datadir="/Users/troy/beerdata"
	lazy val datadir_parts=datadir.split("/")
	def fileLocation(id: Id) = {
		val parts=id.split("/")
		datadir + "/beer/" + parts.mkString("/") + ".xml"
	}
	
	def save[T <: Saveable](item: T, id: Option[Id] = None) = {
		val (itemToSave: Saveable,saveId: Id) = id match {
			case None => (item, item.id.get)
			case Some(theid) => (
				item.dupe(
					id=theid,
					ctime={if (item.ctime.isEmpty) new java.util.Date() else item.ctime.get}
				),
				theid)
		}
		
		/* Make the necessary directories to store the review */
		fileLocation(saveId).split("/").drop(datadir_parts.length).foldLeft(datadir){ (path,item) => 
			val f=new java.io.File(path)
			f.mkdir()
			path + "/" + item
		}
		scala.xml.XML.save(fileLocation(saveId),itemToSave.asXML.head,"UTF-8",true)
	}
}

