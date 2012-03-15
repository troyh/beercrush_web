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
					( Address.xmlTagStreet   , { orig => if (street.isDefined)    <street>{street.get}</street> else orig})
					,(Address.xmlTagCity     , { orig => if (city.isDefined)      <city>{city.get}</city> else orig})
					,(Address.xmlTagState    , { orig => if (state.isDefined)     <state>{state.get}</state> else orig})
					,(Address.xmlTagZip      , { orig => if (zip.isDefined)       <zip>{zip.get}</zip> else orig})
					,(Address.xmlTagCountry  , { orig => if (country.isDefined)   <country>{country.get}</country> else orig})
					,(Address.xmlTagLatitude , { orig => if (latitude.isDefined)  <latitude>{latitude.get}</latitude> else orig})
					,(Address.xmlTagLongitude, { orig => if (longitude.isDefined) <longitude>{longitude.get}</longitude> else orig})
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
	
	private final val xmlTagStreet	 ="street"
	private final val xmlTagCity	 ="city"
	private final val xmlTagState	 ="state"
	private final val xmlTagZip		 ="zip"
	private final val xmlTagCountry	 ="country"
	private final val xmlTagLatitude ="latitude"
	private final val xmlTagLongitude="longitude"
	
	def fromXML(node: xml.NodeSeq) = {
		new Address(
			(node \ xmlTagStreet   ).headOption.map { _.text },
			(node \ xmlTagCity     ).headOption.map { _.text },
			(node \ xmlTagState    ).headOption.map { _.text },
			(node \ xmlTagZip      ).headOption.map { _.text },
			(node \ xmlTagCountry  ).headOption.map { _.text },
			(node \ xmlTagLatitude ).headOption.map { _.text.toDouble },
			(node \ xmlTagLongitude).headOption.map { _.text.toDouble }
		)
	}
}
