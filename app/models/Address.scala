package models

import BeerCrush._
import play.api.libs.json._

case class Address(
	val street			: Option[String] = None,
	val city			: Option[String] = None,
	val state			: Option[String] = None,
	val zip				: Option[String] = None,
	val country			: Option[String] = None,
	val latitude	    : Option[Double] = None,
	val longitude	    : Option[Double] = None
) extends XmlFormat with JsonFormat {
	def asXML = {
		List(street,city,state,zip,country).filter(_.isDefined).size match {
			case 0 => scala.xml.Text("")
			case _ => 
<address>
  { street.map { s => <street>{s}</street> }.getOrElse() }
  { city.map { s => <city>{s}</city> }.getOrElse() }
  { state.map { s => <state>{s}</state> }.getOrElse() }
  { zip.map { s => <zip>{s}</zip> }.getOrElse() }
  { latitude.map { lat => <latitude>{lat}</latitude> }.getOrElse() }
  { longitude.map { lon => <longitude>{lon}</longitude> }.getOrElse() }
  { country.map { s => <country>{s}</country> }.getOrElse() }
</address>
		}
	}
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
