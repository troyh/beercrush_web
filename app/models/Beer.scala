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

	private def transform_base(nodes: NodeSeq, elementNames: Seq[String], foo: (Node, NodeSeq) => Node) = {
		(for (node <- nodes ++ elementNames.filter(e => !nodes.exists(n => e==n.label)).map { e =>
			/* 
				Add empty elements for all the ones in the list above that don't already exist
			*/
			Elem(null,e,Null,xml.TopScope) 
		}) yield node match { 
			/*
				Replace elements with data from this object, keeping elements we don't care about intact
			*/ 
			case Elem(prefix, label, attribs, scope, children @ _*) => foo(node,children)
			case other => other
		}).filter(_.child.length > 0) // Strip out any empty elements
	}

	def transform(nodes: NodeSeq, xpath: String = ""): NodeSeq = transform_base(
		nodes
		,Seq("name","description","abv","ibu","ingredients","grains","hops","yeast","otherings","styles")
		, { (node,children) => 
			xpath + "/" + node.label match { 
				case "/beer"             => <beer id={beerId.getOrElse("").toString}>{transform(children,node.label)}</beer>
				case "/beer/name"        => <name>{name}</name>
				case "/beer/description" => <description>{description.getOrElse("")}</description>
				case "/beer/abv" 	     => <abv>{abv.getOrElse("")}</abv>
				case "/beer/ibu" 	     => <ibu>{ibu.getOrElse("")}</ibu>
				case "/beer/ingredients" => <ingredients>{ingredients.getOrElse("")}</ingredients>
				case "/beer/grains"      => <grains>{grains.getOrElse("")}</grains>
				case "/beer/hops"        => <hops>{hops.getOrElse("")}</hops>
				case "/beer/yeast"       => <yeast>{yeast.getOrElse("")}</yeast>
				case "/beer/otherings"   => <otherings>{otherings.getOrElse("")}</otherings>
				case "/beer/styles"      => 
					<styles>
						{styles.map(_.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>))}
					</styles>
				case _ => node
			}
		}
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
