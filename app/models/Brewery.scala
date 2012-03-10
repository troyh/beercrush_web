package models

import BeerCrush._
import play.api.libs.json._
import controllers._
import scalaj.collection.Imports._

case class Brewery(
	breweryId:	Option[BreweryId],
	val name: 	String,
	val address: Address,
	val phone:	Option[String]
) extends XmlFormat with JsonFormat with Storage.Saveable {
	def id=breweryId
	def descriptiveNameForId = name
	val ctime=None
	def dupe(id:Id,ctime:java.util.Date) = this.copy(breweryId=Some(BreweryId(id))) // TODO: add ctime
	
	lazy val pageURL = { "/" + breweryId.getOrElse("") }
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(doc => Beer.fromExisting(doc.get("id").toString).get)
	}

	def asXML =
		<brewery>
		  <id>{breweryId}</id>
		  <name>{name}‎</name>
		  { address.asXML }
		  { phone.map { s => <phone>{s}</phone>} getOrElse() }
		</brewery>	

	
	def asJson = JsObject((
		Some("id" -> JsString(this.id.toString)) ::
		Some("name" -> JsString(this.name)) :: 
		Some("address" -> this.address.asJson) :: 
		(phone.map { "phone" -> JsString(_) }) ::
		Nil
	).filter(_.isDefined).map(_.get))
}

object Brewery {
	
	def fromExisting(id:BreweryId): Option[Brewery] = {
		try {
			val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/brewery/" + id + ".xml")
			val address=xml \ "address"
			Some(new Brewery(
				(xml \ "id").headOption.map{_.text},
				(xml \ "name").text,
				Address.fromXML(xml \ "address"),
				(xml \ "phone").headOption.map{_.text}
			))
		}
		catch {
			case _ => None
		}
	}
}

