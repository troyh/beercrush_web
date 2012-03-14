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
				orig.child
				,Map(
					("street"    , { orig => if (street.isDefined)    <street>{street.get}</street> else orig})
					,("city"     , { orig => if (city.isDefined)      <city>{city.get}</city> else orig})
					,("state"    , { orig => if (state.isDefined)     <state>{state.get}</state> else orig})
					,("zip"      , { orig => if (zip.isDefined)       <zip>{zip.get}</zip> else orig})
					,("country"  , { orig => if (country.isDefined)   <country>{country.get}</country> else orig})
					,("latitude" , { orig => if (latitude.isDefined)  <latitude>{latitude.get}</latitude> else orig})
					,("longitude", { orig => if (longitude.isDefined) <longitude>{longitude.get}</longitude> else orig})
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
