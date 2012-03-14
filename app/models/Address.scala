package models

import BeerCrush._
import play.api.libs.json._
import scala.xml.NodeSeq

case class Address(
	val street			: Option[String] = None,
	val city			: Option[String] = None,
	val state			: Option[String] = None,
	val zip				: Option[String] = None,
	val country			: Option[String] = None,
	val latitude	    : Option[Double] = None,
	val longitude	    : Option[Double] = None
) extends XmlFormat with JsonFormat {
	def asXML = transform(<address/>)

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes
		,Map(
			("address", { orig => <address>{applyValuesToXML(
				orig
				,Map(
					("street", { orig => <street>{street}</street>})
					,("city", { orig => <city>{city}</city>})
					,("state", { orig => <state>{state}</state>})
					,("zip", { orig => <zip>{zip}</zip>})
					,("country", { orig => <country>{country}</country>})
					,("latitude", { orig => <latitude>{latitude}</latitude>})
					,("longitude", { orig => <longitude>{longitude}</longitude>})
				)
				)}</address>}
			)
		)
	)
	
	def asJson = JsObject(
		(
			street.map("street" -> JsString(_)) ::
			city.map { "city" -> JsString(_) } ::
			state.map { "state" -> JsString(_) } ::
			zip.map { "zip" -> JsString(_) } ::
			latitude.map { "latitude" -> JsNumber(_) } :: 
			longitude.map { "longitude" -> JsNumber(_) } :: 
			country.map { "country" -> JsString(_) } :: 
			Nil
		).filter(_.isDefined).map(_.get)
	)
}
object Address {
	def fromXML(node: xml.NodeSeq) = {
		new Address(
			(node \ "street").headOption.map { _.text },
			(node \ "city").headOption.map { _.text },
			(node \ "state").headOption.map { _.text },
			(node \ "zip").headOption.map { _.text },
			(node \ "country").headOption.map { _.text },
			(node \ "latitude").headOption.map { _.text.toDouble },
			(node \ "longitude").headOption.map { _.text.toDouble }
		)
	}
}
