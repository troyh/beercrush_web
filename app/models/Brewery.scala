package models

import BeerCrush._
import play.api.libs.json._
import controllers._
import scalaj.collection.Imports._

case class Brewery(
	breweryId:	BreweryId,
	val name: 	String,
	val address: Address,
	val phone:	Option[String]
) extends PersistentObject(Some(breweryId)) with JsonFormat {
	lazy val pageURL = { "/" + id }
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(doc => Beer.fromExisting(Some(doc.get("id").toString)).get)
	}

	def save: Unit = {
		val thisId=this.id match {
			case id if (id.isEmpty) => {
				/* Make up an ID for this */
				"[^a-zA-Z0-9]+".r.replaceAllIn("['\"]+".r.replaceAllIn(this.name,""),"-") 
			}
			case _ => { this.id }
		}
		val xml=
		<brewery>
		  <id>{thisId}</id>
		  <name>{this.name}‎</name>
		  { address.asXML }
		  { phone.map { s => <phone>{s}</phone>} getOrElse() }
		</brewery>	
		
		// scala.xml.XML.loadString(xml.toString)
		scala.xml.XML.save("/Users/troy/beerdata/editedBrewery.xml",xml,"UTF-8",true)
	}
	
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
				(xml \ "id").text,
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

