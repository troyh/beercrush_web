package models

import BeerCrush._
import play.api.libs.json._
import scala.xml._
import scala.annotation._

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
	lazy val brewery = {
		val bid: Option[BreweryId]=beerId.map(_.breweryId)
		// val bid: BreweryId = b.breweryId
		bid match {
			case None => None
			case Some(id) => Brewery.fromExisting(id)
		}
	}

	def asXML=transform(<beer/>).head
	
	def transform(nodes: NodeSeq, xpath: Seq[String] = Seq()): NodeSeq = {
		(xpath match {
			case Seq() => {
				for (node <- nodes ++ Seq("beer").filter(e => !nodes.exists(n => e==n.label)).map { e =>
						Elem(null,e,Null,xml.TopScope)
				}) yield node match {
					case Elem(prefix, label, attribs, scope, children @ _*) => label match {
						case "beer" => <beer id={beerId.get.toString}>{transform(children,Seq(label))}</beer>
						case _ => node
					}
					case other => other
				}
			}
			case Seq("beer") => {
				for (node <- nodes ++ Seq("name","description","abv","ibu","ingredients","grains","hops","yeast","otherings","styles").filter(e => !nodes.exists(n => e==n.label)).map { e =>
						Elem(null,e,Null,xml.TopScope)
				}) yield node match {
					case Elem(prefix, label, attribs, scope, children @ _*) => label match {
						case "name" => <name>{name}</name>
						case "description" if (description.isDefined) => <description>{description.get}</description>
						case "abv" if (abv.isDefined) => <abv>{abv.get}</abv>
						case "ibu" if (ibu.isDefined) => <ibu>{ibu.get}</ibu>
						case "ingredients" if (ingredients.isDefined) => <ingredients>{ingredients.get}</ingredients>
						case "grains" if (grains.isDefined) => <grains>{grains.get}</grains>
						case "hops" if (hops.isDefined) => <hops>{hops.get}</hops>
						case "yeast" if (yeast.isDefined) => <yeast>{yeast.get}</yeast>
						case "otherings" if (otherings.isDefined) => <otherings>{otherings.get}</otherings>
						case "styles" if (styles.isDefined) => 
							<styles>
								{styles.map(_.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>))}
							</styles>
						case _ => node
					}
					case other => other
				}
			}
		}).filter(elem => elem.child.length > 0 )
	}
	
	def asJson = JsObject(
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
		try {
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(beerId))
			Some(Beer(
				beerId = Some(beerId),
				name = (xml \ "name").headOption.map{_.text.trim}.getOrElse(""),
				description = (xml \ "description").headOption.map{_.text.trim},
				abv = (xml \ "abv").headOption.map{_.text.toDouble},
				ibu = try { (xml \ "ibu").headOption.map{ _.text.toInt } } catch { case _ => None },
				ingredients = (xml \ "ingredients").headOption.map{_.text.trim},
				grains = (xml \ "grains").headOption.map{_.text.trim},
				hops = (xml \ "hops").headOption.map{_.text.trim},
				yeast = (xml \ "yeast").headOption.map{_.text.trim},
				otherings = (xml \ "otherings").headOption.map{_.text.trim},
				styles = Some((xml \ "styles").map( style => 
					new BeerStyle((style \ "style" \ "bjcp_style_id").text,(style \ "style" \ "name").text.trim)
				).toList)
			))
		}
		catch {
			case _ => None
		}
	}
}
