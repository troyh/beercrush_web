package models

import BeerCrush._
import play.api.libs.json._
import scala.xml._
import scala.annotation._

/**
  * A representation for a beer. Beers are things that [[models.Brewery]]s make 
  * and what Places serve. Most properties are optional.
  * 
  * @param beerId The unique ID of the beer, i.e., "Alaskan-Brewing-Co/Esb"
  * @param name The name of the beer, i.e., "Alaskan ESB"
  * @param description A description of the beer, usually from the brewer
  * @param abv Alcohol By Volume, a percentage of alcohol in the beer
  * @param ibu International Bittering Units (IBUs) of the beer
  * @param ingredients Any text describing the ingredients used to make the beer
  * @param grains Any text describing the specific grains used to make the beer
  * @param hops Any text describing the hops used to make the beer
  * @param yeast Any text describing the yeast used to make the beer
  * @param otherings Any text describing the ingredients used to make the beer that don't fit into the above categories
  * @param styles Style(s) of the beer
  */
case class Beer(
	beerId:			Option[BeerId],
	val name: 		String,
	val description:Option[String],
	val abv: 		Option[Double],
	val ibu: 		Option[Int],
	val ingredients:Option[String],
	val grains:		Option[String],
	val hops:		Option[String],
	val yeast:		Option[String],
	val otherings:	Option[String],
	val styles: 	Option[List[BeerStyle]]
) extends XmlFormat with JsonFormat with Storage.Saveable {
	def id=beerId
	val ctime: Option[java.util.Date] = None
	def descriptiveNameForId = name
	def dupe(id:Id,ctime:java.util.Date) = this.copy(beerId=Some(BeerId(id))) // TODO: add ctime
	
	lazy val pageURL = { "/" + beerId.get }

	/**
	  * The Brewery object for the brewer of this beer
	  */
	lazy val brewery = {
		val bid: Option[BreweryId]=beerId.map(_.breweryId)
		bid match {
			case None => None
			case Some(id) => Brewery.fromExisting(id)
		}
	}

	def toXML= transform(<beer/>)
	
	private def replaceChildTextElem(elem: Elem, s: String) = elem.copy(child=for (e <- elem.child) yield e match {
			case t @ <text>{_*}</text> => t.asInstanceOf[Elem].copy(child=Text(s))
			case other => other
	})

	def transform(nodes: NodeSeq): NodeSeq = {
		for (n <- nodes) yield n match {
			case b @ <beer>{kids @ _*}</beer> => b.asInstanceOf[Elem] % Attribute("","id",beerId.getOrElse("").toString,Null) % Attribute("","breweryid",brewery.get.breweryId.getOrElse("").toString,Null) copy(
				child=for (k <- kids) yield k match {
					case <name>{_*}</name>                  => k.asInstanceOf[Elem].copy(child=Text(name))
					case <id>{_*}</id>                      => <id/>         // Deletes it
					case <brewery_id>{_*}</brewery_id>      => <brewery_id/> // Deletes it
					case <abv>{_*}</abv> if (abv.isDefined) => k.asInstanceOf[Elem] % Attribute("","value",abv.get.toString,Null)
					case <ibu>{_*}</ibu> if (ibu.isDefined) => k.asInstanceOf[Elem] % Attribute("","value",ibu.get.toString,Null)
					case <description>{_*}</description>    if (description.isDefined) => k.asInstanceOf[Elem].copy(child=Text(description.get)) 
					case <ingredients>{_*}</ingredients>    if (ingredients.isDefined) => replaceChildTextElem(k.asInstanceOf[Elem],ingredients.get)
					case <grains>{_*}</grains>              if (grains.isDefined)      => replaceChildTextElem(k.asInstanceOf[Elem],grains.get)
					case <hops>{_*}</hops>                  if (hops.isDefined)        => replaceChildTextElem(k.asInstanceOf[Elem],hops.get)
					case <yeast>{_*}</yeast>                if (yeast.isDefined)       => replaceChildTextElem(k.asInstanceOf[Elem],yeast.get)
					case <otherings>{_*}</otherings>        if (otherings.isDefined)   => replaceChildTextElem(k.asInstanceOf[Elem],otherings.get)
					case other => other
				}
			)
			case other => other
		}
	}

	def toJSON = JsObject(
		(
		  beerId.map{"id" -> JsString(_)} ::
		  brewery.map{b => "brewery" -> JsString(b.breweryId)} ::
		  Some("name" -> JsString(name)) ::
		  description.map{"description" -> JsString(_)} ::
		  styles.map{ss => "styles" -> JsArray(ss.map(s => JsObject(List(
			  "id" -> JsString(s.id),
			  "name" -> JsString(s.name)
		  ))))} ::
		  abv.map{"abv" -> JsNumber(_)} ::
		  ibu.map{"ibu" -> JsNumber(_)} ::
		  Nil
		).filter(_.isDefined).map{_.get}
	)
}

object Beer {
	
	def fromExisting(beerId:BeerId): Option[Beer] = {
		val xml=scala.xml.XML.loadFile(Storage.fileLocation(beerId))
		try {
			Some(Beer(
				beerId      = Some(beerId),
				name        = (xml \ "name"       ).headOption.map{_.text.trim}.getOrElse(""),
				description = (xml \ "description").headOption.map{_.text.trim},
				abv         = (xml \ "abv"        ).flatMap(e => e.attribute("value").map(v=>v.text) ++ Seq(e.text)).map(x => try {x.toDouble} catch { case _ => 0.0}).filter(_ > 0.0).headOption,
				ibu         = (xml \ "ibu"        ).flatMap(e => e.attribute("value").map(v=>v.text) ++ Seq(e.text)).map(x => try {x.toDouble} catch { case _ => 0.0}).map(_.toInt).filter(_ > 0).headOption,
				ingredients = (xml \ "ingredients").flatMap(e => Seq((e \ "text").text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				grains      = (xml \ "grains"     ).flatMap(e => Seq((e \ "text").text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				hops        = (xml \ "hops"       ).flatMap(e => Seq((e \ "text").text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				yeast       = (xml \ "yeast"      ).flatMap(e => Seq((e \ "text").text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				otherings   = (xml \ "otherings"  ).flatMap(e => Seq((e \ "text").text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				styles = Some((xml \ "styles"     ).map( style => 
					new BeerStyle((style \ "style" \ "bjcp_style_id").text,(style \ "style" \ "name").text.trim)
				).toList)
			))
		}
		catch {
			case _ => None
		}
	}
}
