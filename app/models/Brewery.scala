package models

import BeerCrush._
import play.api.libs.json._
import controllers._
import scalaj.collection.Imports._
import scala.xml._

case class Brewery(
	val id:	BreweryId,
	val name: 	String,
	val address: Address,
	val phone:	Option[String]
) extends XmlFormat with JsonFormat {
	
	lazy val pageURL = { "/" + id }
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(doc => Beer(doc.get("id").toString).get)
	}

	def toXML = transform(<brewery/>)

	import SuperNode._
	def transform(nodes: NodeSeq): NodeSeq = for (node <- nodes) yield node match {
		case b @ <brewery>{_*}</brewery> => b.asInstanceOf[Elem] % Attribute("","id",id.toString,Null) copy(child=for (k <- b.withMissingChildElements(Seq("name","vcard","phone")).child) yield k match {
			case <id>{_*}</id>           => <id/> // Deletes it on Storage.save()
			case <name>{_*}</name> 	     => k.asInstanceOf[Elem].copy(child=Text(name))
			case <address>{_*}</address> => <address/> // Deletes it on Storage.save()
			case <vcard>{_*}</vcard> 	 => address.transform(k).head
			case <phone>{_*}</phone> 	 => k.asInstanceOf[Elem].copy(child=Text(phone.get))
			case other => other
		})
		case other => other
	}
	
	def toJSON = JsObject((
		Some("id" -> JsString(this.id.toString)) ::
		Some("name" -> JsString(this.name)) :: 
		Some("vcard" -> this.address.toJSON) :: 
		(phone.map { "phone" -> JsString(_) }) ::
		Nil
	).filter(_.isDefined).map(_.get))
}

object Brewery {
	def apply(id:BreweryId): Option[Brewery] = {
		try {
			val xml=scala.xml.XML.loadFile(BeerCrush.datadir + "/brewery/" + id + ".xml")
			(xml \ "@id").headOption.map{s => BreweryId(s.text)} match {
				case None => None
				case Some(breweryId: BreweryId) if (id == breweryId) => {
					val address=(xml \ "vcard" \ "adr") match {
						case vcard:NodeSeq if (vcard.length > 0)=> vcard.head
						case _ => (xml \ "address").head
					}
					Some(Brewery(
						id,
						(xml \ "name").text,
						Address.fromXML(address),
						(xml \ "phone").headOption.map{_.text}
					))
				}
			}
		}
		catch {
			case _ => None
		}
	}
}

