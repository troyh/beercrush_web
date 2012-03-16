package models

import BeerCrush._
import play.api.libs.json._
import controllers._
import scalaj.collection.Imports._
import scala.xml._

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

	def toXML = transform(<brewery/>)
	
	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes
		,Map(
			(Brewery.xmlTagBrewery, { orig => <brewery id={breweryId.getOrElse("").toString}>{applyValuesToXML(
				orig.child
				,Map(
					( Brewery.xmlTagId   , { orig => <id/> } ) // Effectively deletes it
					,(Brewery.xmlTagName , { orig => <name>{name}</name>})
					,(Brewery.xmlTagAddress, { orig => <address/> }) // Effectively deletes it
					,(Brewery.xmlTagVcard, { orig => address.transform(orig) })
					,(Brewery.xmlTagPhone, { orig => phone match {
						case Some(phone) => <phone>{phone}</phone>
						case None => orig 
					}})
				)
				)}</brewery>}
			)
		)
	)
	
	def toJSON = JsObject((
		Some(Brewery.xmlTagId -> JsString(this.id.toString)) ::
		Some(Brewery.xmlTagName -> JsString(this.name)) :: 
		Some(Brewery.xmlTagAddress -> this.address.toJSON) :: 
		(phone.map { Brewery.xmlTagPhone -> JsString(_) }) ::
		Nil
	).filter(_.isDefined).map(_.get))
}

object Brewery {
	
	private final val xmlTagBrewery="brewery"
	private final val xmlTagId="id"
	private final val xmlAttributeId="@" + xmlTagId
	private final val xmlTagName="name"
	private final val xmlTagAddress="address" // @deprecated
	private final val xmlTagVcard="vcard"
	private final val xmlTagPhone="phone"
	
	def fromExisting(id:BreweryId): Option[Brewery] = {
		try {
			val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/brewery/" + id + ".xml")
			val address=(xml \ xmlTagVcard \ "adr") match {
				case vcard:NodeSeq if (vcard.length > 0)=> vcard.head
				case _ => (xml \ xmlTagAddress).head
			}
			Some(Brewery(
				(xml \ Brewery.xmlAttributeId).headOption.map{_.text},
				(xml \ Brewery.xmlTagName).text,
				Address.fromXML(address),
				(xml \ Brewery.xmlTagPhone).headOption.map{_.text}
			))
		}
		catch {
			case _ => None
		}
	}
}

