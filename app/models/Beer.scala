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

	def asXML= transform(<beer/>)

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes
		,Map(
			("beer", { orig =>
				<beer id={beerId.getOrElse("").toString}>{ 
					applyValuesToXML(
						orig.child
						,Map(
							("name"        , { orig => <name>{name}</name> } )
							,("id"		   , { orig => <id/> } ) // Effectively deletes it
							,("description", { orig => if (description.isDefined) <description>{description.get}</description> else orig } )
							,("abv" 	   , { orig => if (abv.isDefined) <abv>{abv.get}</abv> else orig } )
							,("ibu" 	   , { orig => if (ibu.isDefined) <ibu>{ibu.get}</ibu> else orig } )
							,("ingredients", { orig => if (ingredients.isDefined) <ingredients>{ingredients.get}</ingredients> else orig } )
							,("grains"     , { orig => if (grains.isDefined) <grains>{grains.get}</grains> else orig } )
							,("hops"       , { orig => if (hops.isDefined) <hops>{hops.get}</hops> else orig } )
							,("yeast"      , { orig => if (yeast.isDefined) <yeast>{yeast.get}</yeast> else orig } )
							,("otherings"  , { orig => if (otherings.isDefined) <otherings>{otherings.get}</otherings> else orig } )
							,("styles"     , { orig => 
								if (styles.isDefined && styles.get.length > 0) {
									<styles>
										{styles.map(_.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>))}
									</styles>
								} else
									orig
							})
						)
					)
				}</beer>
			})
		)
	)
	
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
				abv = try { (xml \ "abv").headOption.map{_.text.toDouble} } catch { case _ => None },
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
