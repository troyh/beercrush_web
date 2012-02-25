package models

import BeerCrush._
import play.api.libs.json._

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
) extends PersistentObject(beerId) with JsonFormat {
	lazy val pageURL = { "/" + id }
	lazy val brewery = {
		val bid: Option[BreweryId]=beerId.map(_.breweryId)
		// val bid: BreweryId = b.breweryId
		bid match {
			case None => None
			case Some(id) => Brewery.fromExisting(id)
		}
	}

	def save = {
		val theBeerId=beerId match {
			case None => /* Make up an ID */ "[^a-zA-Z0-9]+".r.replaceAllIn(name.replace("'",""),"-")
			case Some(id) => id
		}
		val xml=
		<beer>
		  <id>{theBeerId}</id>
		  { brewery.map{ b => <brewery_id>{b.breweryId}</brewery_id>}.getOrElse() }
		  <calories_per_ml></calories_per_ml>
		  { abv.map{ abv => <abv>{abv}</abv>}.getOrElse() }
		  { ibu.map{ ibu => <ibu>{ibu}</ibu>}.getOrElse() }
		  <name>{name}</name>
		  { description.map{ s => <description>{s}</description>}.getOrElse() }
		  <availability></availability>
		  { ingredients.map{ s => <ingredients>{s}</ingredients>}.getOrElse() }
		  { grains.map{ s => <grains>{s}</grains>}.getOrElse() }
		  { hops.map{ s => <hops>{s}</hops>}.getOrElse() }
		  { yeast.map{ s => <yeast>{s}</yeast>}.getOrElse() }
		  { otherings.map{ s => <otherings>{s}</otherings>}.getOrElse() }
		  <styles>
		  			  {styles.map(_.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>))}
		  </styles>
		</beer>
		
		scala.xml.XML.loadString(xml.toString)
		scala.xml.XML.save("/Users/troy/beerdata/editedBeer.xml",xml,"UTF-8",true)
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
	def fromExisting(id:Option[BeerId]): Option[Beer] = {
		
		id match {
			case None => None
			case Some(beerId) => {
				val xml=scala.xml.XML.loadFile(PersistentObject.fileLocationFromId(beerId))
				Some(Beer(
					beerId = id,
					name = (xml \ "name").text,
					description = (xml \ "description").headOption.map{_.text},
					abv = (xml \ "abv").headOption.map{_.text.toDouble},
					ibu = (xml \ "ibu").headOption.map{_.text.toInt},
					ingredients = (xml \ "ingredients").headOption.map{_.text},
					grains = (xml \ "grains").headOption.map{_.text},
					hops = (xml \ "hops").headOption.map{_.text},
					yeast = (xml \ "yeast").headOption.map{_.text},
					otherings = (xml \ "otherings").headOption.map{_.text},
					styles = Some((xml \ "styles").map( style => 
						new BeerStyle((style \ "style" \ "bjcp_style_id").text,(style \ "style" \ "name").text)
					).toList)
				))
			}
		}
	}
}
