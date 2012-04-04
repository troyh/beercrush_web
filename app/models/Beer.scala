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
	val id:			BeerId,
	val name: 		String,
	val description:Option[String],
	val abv: 		Option[Double],
	val ibu: 		Option[Int],
	val ingredients:Option[String],
	val grains:		Option[String],
	val hops:		Option[String],
	val yeast:		Option[String],
	val otherings:	Option[String],
	val styles: 	Option[List[StyleId]]
) extends XmlFormat with JsonFormat {
	// def id=beerId
	// val ctime: Option[java.util.Date] = None
	// def descriptiveNameForId = name
	// def dupe(id:Id,ctime:java.util.Date) = this.copy(beerId=BeerId(id)) // TODO: add ctime
	
	lazy val pageURL = { "/" + id }
	lazy val beerStyles = BeerStyle.getObjects(styles.getOrElse(List()))
	
	/**
	  * The Brewery object for the brewer of this beer
	  */
	lazy val brewery = Brewery(id.breweryId)

	def toXML= transform(<beer/>)

	import SuperNode._
	def transform(nodes: NodeSeq): NodeSeq = for (n <- nodes) yield n match {
		case b @ <beer>{kids @ _*}</beer> => 
			b.asInstanceOf[Elem] % 
				Attribute("","id",id.toString,Null) % 
				Attribute("","breweryid",brewery.get.id.toString,Null) copy(
			child=for (k <- b.withMissingChildElements(Seq("abv","ibu","description","ingredients","grains","hops","yeast","otherings","styles")).child) yield k match {
				case <name>{_*}</name>                  => k.asInstanceOf[Elem].copy(child=Text(name))
				case <id>{_*}</id>                      => <id/>         // Deletes it
				case <brewery_id>{_*}</brewery_id>      => <brewery_id/> // Deletes it
				case <abv>{_*}</abv> if (abv.isDefined) => k.asInstanceOf[Elem] % Attribute("","value",abv.get.toString,Null)
				case <ibu>{_*}</ibu> if (ibu.isDefined) => k.asInstanceOf[Elem] % Attribute("","value",ibu.get.toString,Null)
				case <description>{_*}</description>    => k.asInstanceOf[Elem].copy(child=Text(description.getOrElse(""))) 
				case <ingredients>{_*}</ingredients>    => replaceChildTextElem(k.asInstanceOf[Elem],ingredients.getOrElse(""))
				case <grains>{_*}</grains>              => replaceChildTextElem(k.asInstanceOf[Elem],grains.getOrElse(""))
				case <hops>{_*}</hops>                  => replaceChildTextElem(k.asInstanceOf[Elem],hops.getOrElse(""))
				case <yeast>{_*}</yeast>                => replaceChildTextElem(k.asInstanceOf[Elem],yeast.getOrElse(""))
				case <otherings>{_*}</otherings>        => replaceChildTextElem(k.asInstanceOf[Elem],otherings.getOrElse(""))
				case <styles>{_*}</styles>              => styles.map(_.map(id => <style id="{id}"/>)).head.head
				case other => other
			})
		case other => other
	}

	def toJSON = JsObject(
		(
		  Some("id" -> JsString(id.toString)) ::
		  brewery.map{b => "brewery" -> JsString(b.id.toString)} ::
		  Some("name" -> JsString(name)) ::
		  description.map{"description" -> JsString(_)} ::
		  styles.map{ss => "styles" -> JsArray(ss.map(id => JsObject(List(
			  "id" -> JsString(id.toString)
		  ))))} ::
		  abv.map{"abv" -> JsNumber(_)} ::
		  ibu.map{"ibu" -> JsNumber(_)} ::
		  Nil
		).filter(_.isDefined).map{_.get}
	)
}

object Beer {
	def apply(id: BeerId): Option[Beer] = {
		val xml=scala.xml.XML.loadFile(id.fileLocation)
		try {
			Some(Beer(
				id      	= id,
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
					new StyleId(style \ "style" \ "bjcp_style_id" text)
				).toList)
			))
		}
		catch {
			case _ => None
		}
	}
}
