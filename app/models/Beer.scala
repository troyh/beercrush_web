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
		// val bid: BreweryId = b.breweryId
		bid match {
			case None => None
			case Some(id) => Brewery.fromExisting(id)
		}
	}

	def toXML= transform(<beer/>)

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes
		,Map(
			("beer", { orig =>
				<beer id={beerId.getOrElse("").toString}>{ 
					applyValuesToXML(
						orig.child
						,Map(
							( Beer.xmlTagName       , { orig => <name>{name}</name> } )
							,(Beer.xmlTagId		    , { orig => <id/> } ) // Effectively deletes it
							,(Beer.xmlTagDescription, { orig => if (description.isDefined) <description>{description.get}</description> else orig } )
							,(Beer.xmlTagAbv 	    , { orig => if (abv.isDefined) <abv>{abv.get}</abv> else orig } )
							,(Beer.xmlTagIbu 	    , { orig => if (ibu.isDefined) <ibu>{ibu.get}</ibu> else orig } )
							,(Beer.xmlTagIngredients, { orig => if (ingredients.isDefined) <ingredients>{ingredients.get}</ingredients> else orig } )
							,(Beer.xmlTagGrains     , { orig => if (grains.isDefined) <grains>{grains.get}</grains> else orig } )
							,(Beer.xmlTagHops       , { orig => if (hops.isDefined) <hops>{hops.get}</hops> else orig } )
							,(Beer.xmlTagYeast      , { orig => if (yeast.isDefined) <yeast>{yeast.get}</yeast> else orig } )
							,(Beer.xmlTagOtherings  , { orig => if (otherings.isDefined) <otherings>{otherings.get}</otherings> else orig } )
							,(Beer.xmlTagStyles     , { orig => 
								if (styles.isDefined && styles.get.length > 0) {
									<styles>
										{styles.get.map(style => 
											<style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>
										)}
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
	
	private final val xmlTagName= "name"
	private final val xmlTagId="id"
	private final val xmlTagDescription="description"
	private final val xmlTagAbv="abv"
	private final val xmlTagIbu="ibu"
	private final val xmlTagIngredients="ingredients"
	private final val xmlTagGrains="grains"
	private final val xmlTagHops="hops"
	private final val xmlTagYeast="yeast"
	private final val xmlTagOtherings="otherings"
	private final val xmlTagStyles="styles"
	private final val xmlTagStyle="style"
	private final val xmlTagStyleName="name"
	private final val xmlTagBJCPStyleId="bjcp_style_id"
	
	def fromExisting(beerId:BeerId): Option[Beer] = {
		try {
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(beerId))
			Some(Beer(
				beerId = Some(beerId),
				name =        (xml \ xmlTagName       ).headOption.map{_.text.trim}.getOrElse(""),
				description = (xml \ xmlTagDescription).headOption.map{_.text.trim},
				abv = try {   (xml \ xmlTagAbv        ).headOption.map{_.text.toDouble} } catch { case _ => None },
				ibu = try {   (xml \ xmlTagIbu		  ).headOption.map{ _.text.toInt } } catch { case _ => None },
				ingredients = (xml \ xmlTagIngredients).headOption.map{_.text.trim},
				grains =      (xml \ xmlTagGrains	  ).headOption.map{_.text.trim},
				hops =        (xml \ xmlTagHops	      ).headOption.map{_.text.trim},
				yeast = 	  (xml \ xmlTagYeast	  ).headOption.map{_.text.trim},
				otherings =   (xml \ xmlTagOtherings  ).headOption.map{_.text.trim},
				styles = Some((xml \ xmlTagStyles     ).map( style => 
					new BeerStyle((style \ xmlTagStyle \ xmlTagBJCPStyleId).text,(style \ xmlTagStyle \ xmlTagStyleName).text.trim)
				).toList)
			))
		}
		catch {
			case _ => None
		}
	}
}
