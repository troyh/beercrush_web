package models

import BeerCrush._
import play.api.libs.json._
import scala.xml.{NodeSeq, Elem, Text}

/**
  * Represents a postal address. 
  *
  * @param street	Street address
  * @param city	    City/Municipality
  * @param state	State/Region
  * @param zip		Zip code/Postal code
  * @param country	Country code
  * @param latitude Latitude of address
  * @param longitude Longitude of address
  *
  */
case class Address(
	val street			: Option[String] = None,
	val city			: Option[String] = None,
	val state			: Option[String] = None,
	val zip				: Option[String] = None,
	val country			: Option[String] = None,
	val latitude	    : Option[Double] = None,
	val longitude	    : Option[Double] = None
) extends XmlFormat with JsonFormat {
	
	/**
    The XML format is a subset of [[http://en.wikipedia.org/wiki/VCard xCard]]. Sample XML:

    {{{
    <brewery id="Alaskan-Brewing-Co">
      <vcard>
        <adr>
          <code>99801-9540</code>
          <latitude>58.356871</latitude>
          <street>5429 Shaune Dr</street>
          <region>AK</region>
          <locality>Juneau</locality>
          <longitude>-134.489903</longitude>
          <country>US</country>
        </adr>
      </vcard>
      <phone>(907) 780-5866</phone>
      <name>Alaskan Brewing Co&#x200E;</name>
    </brewery>
    }}}

	*/
	def toXML = transform(<address/>)

	import SuperNode._
	def transform(nodes: NodeSeq): NodeSeq = for (n <- nodes) yield n match {
		case v @ <vcard>{kids @ _*}</vcard> => v.asInstanceOf[Elem].copy(child=for (k <- kids) yield k match {
			case a @ <adr>{_*}</adr> => a.asInstanceOf[Elem].copy(child=for (k <- a.withMissingChildElements(Seq("code","latitude","longitude","street","region","locality","country")).child) yield k match {
				case <code>{_*}</code>           if (zip.isDefined)       => k.asInstanceOf[Elem].copy(child=Text(zip.get))
				case <latitude>{_*}</latitude>   if (latitude.isDefined)  => k.asInstanceOf[Elem].copy(child=Text(latitude.get.toString))
				case <longitude>{_*}</longitude> if (longitude.isDefined) => k.asInstanceOf[Elem].copy(child=Text(longitude.get.toString))
				case <street>{_*}</street> 	     if (street.isDefined)    => k.asInstanceOf[Elem].copy(child=Text(street.get))
				case <region>{_*}</region>       if (city.isDefined)      => k.asInstanceOf[Elem].copy(child=Text(state.get))
				case <locality>{_*}</locality>   if (city.isDefined)      => k.asInstanceOf[Elem].copy(child=Text(city.get))
				case <country>{_*}</country>     if (country.isDefined)   => k.asInstanceOf[Elem].copy(child=Text(country.get))
			})
		})
	}

	def toJSON = JsObject(
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

	private final val xmlTagVcard    ="vcard"	
	private final val xmlTagAdr      ="adr"
	private final val xmlTagStreet	 ="street"
	private final val xmlTagLocality ="locality"
	private final val xmlTagRegion	 ="region"
	private final val xmlTagPostCode ="code"
	private final val xmlTagCountry	 ="country"
	private final val xmlTagLatitude ="latitude"
	private final val xmlTagLongitude="longitude"
	private final val xmlTagTel      ="tel"
	
	def fromXML(node: xml.NodeSeq) = {
		Address(
			(node \ xmlTagStreet   ).headOption.map { _.text },
			(node \ xmlTagLocality ).headOption.map { _.text }.orElse { (node \ "city" ).headOption.map { _.text } }, 
			(node \ xmlTagRegion   ).headOption.map { _.text }.orElse { (node \ "state").headOption.map { _.text } },
			(node \ xmlTagPostCode ).headOption.map { _.text }.orElse { (node \ "zip"  ).headOption.map { _.text } },
			(node \ xmlTagCountry  ).headOption.map { _.text },
			(node \ xmlTagLatitude ).headOption.map { _.text.toDouble },
			(node \ xmlTagLongitude).headOption.map { _.text.toDouble }
		)
	}
}
