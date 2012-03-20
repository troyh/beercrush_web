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

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes
		,Map(
			("beer", { orig =>
				<beer id={beerId.getOrElse("").toString} breweryid={brewery.get.breweryId.getOrElse("").toString}>{ 
					applyValuesToXML(
						orig.child
						,Map(
							( Beer.xmlTagName       , { orig => <name>{name}</name> } )
							,(Beer.xmlTagId		    , { orig => <id/> } ) // Effectively deletes it
							,("brewery_id"		    , { orig => <brewery_id/> } ) // Effectively deletes it
							,(Beer.xmlTagDescription, { orig => if (description.isDefined) <description>{description.get}</description> else orig } )
							,(Beer.xmlTagAbv 	    , { orig => if (abv.isDefined) <abv/> % Attribute("",Beer.xmlTagValue,abv.get.toString,Null) else orig } )
							,(Beer.xmlTagIbu 	    , { orig => if (ibu.isDefined) <ibu/> % Attribute("",Beer.xmlTagValue,ibu.get.toString,Null) else orig } )
							,(Beer.xmlTagIngredients, { orig => 
								<ingredients>{ 
									applyValuesToXML(
										orig.child
										,Map(
											(Beer.xmlTagText, orig => if (ingredients.isDefined) <text>{ingredients.get}</text> else orig)
										)
									)
								}</ingredients>
							} )
							,(Beer.xmlTagGrains     , { orig => 
								<grains>{ 
									applyValuesToXML(
										orig.child
										,Map(
											(Beer.xmlTagText, orig => if (grains.isDefined) <text>{grains.get}</text> else orig)
										)
									)
								}</grains>
							} )
							,(Beer.xmlTagHops       , { orig => 
								if (hops.isDefined) <hops><text>{hops.get}</text></hops> else orig 
								<hops>{ 
									applyValuesToXML(
										orig.child
										,Map(
											(Beer.xmlTagText, orig => if (hops.isDefined) <text>{hops.get}</text> else orig)
										)
									)
								}</hops>
							} )
							,(Beer.xmlTagYeast      , { orig => 
								<yeast>{ 
									applyValuesToXML(
										orig.child
										,Map(
											(Beer.xmlTagText, orig => if (yeast.isDefined) <text>{yeast.get}</text> else orig)
										)
									)
								}</yeast>
							} )
							,(Beer.xmlTagOtherings  , { orig => 
								if (otherings.isDefined) <otherings><text>{otherings.get}</text></otherings> else orig 
								<otherings>{ 
									applyValuesToXML(
										orig.child
										,Map(
											(Beer.xmlTagText, orig => if (otherings.isDefined) <text>{otherings.get}</text> else orig)
										)
									)
								}</otherings>
							} )
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
	private final val xmlTagValue="value"
	private final val xmlAttribValue="@" + xmlTagValue
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
	private final val xmlTagText="text"
	
	def fromExisting(beerId:BeerId): Option[Beer] = {
		val xml=scala.xml.XML.loadFile(Storage.fileLocation(beerId))
		try {
			Some(Beer(
				beerId = Some(beerId),
				name =        (xml \ xmlTagName       ).headOption.map{_.text.trim}.getOrElse(""),
				description = (xml \ xmlTagDescription).headOption.map{_.text.trim},
				abv =         (xml \ xmlTagAbv).flatMap(e => e.attribute(xmlTagValue).map(v=>v.text) ++ Seq(e.text)).map(x => try {x.toDouble} catch { case _ => 0.0}).filter(_ > 0.0).headOption,
				ibu =         (xml \ xmlTagIbu).flatMap(e => e.attribute(xmlTagValue).map(v=>v.text) ++ Seq(e.text)).map(x => try {x.toDouble} catch { case _ => 0.0}).map(_.toInt).filter(_ > 0).headOption,
				ingredients = (xml \ xmlTagIngredients).flatMap(e => Seq((e \ xmlTagText).text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				grains      = (xml \ xmlTagGrains     ).flatMap(e => Seq((e \ xmlTagText).text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				hops        = (xml \ xmlTagHops       ).flatMap(e => Seq((e \ xmlTagText).text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				yeast       = (xml \ xmlTagYeast      ).flatMap(e => Seq((e \ xmlTagText).text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
				otherings   = (xml \ xmlTagOtherings  ).flatMap(e => Seq((e \ xmlTagText).text.trim) ++ Seq(e.text.trim)).filter(_.length > 0).headOption,
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
