package models

import BeerCrush._
import Storage._
import scala.collection.immutable.{Range, NumericRange}
import scala.annotation._
import play.api.libs.json._
import scala.xml.{Attribute, Null, Elem}

case class StyleId(styleId: String) extends Id(styleId) {
	def fileLocation = StyleId.fileLocation
}

object StyleId {
	implicit def string2StyleId(s: String) = StyleId(s)
	def fileLocation = BeerCrush.datadir + "/beerstyles.xml"
	object Undefined extends StyleId("")
}

case class BeerStyle(
	val styleId: StyleId, 
	val name: String,
	val abv: Option[NumericRange[Double]] = None,
	val ibu: Option[NumericRange.Inclusive[Int]] = None,
	val og : Option[NumericRange[Double]] = None,
	val fg : Option[NumericRange[Double]] = None,
	val srm: Option[NumericRange.Inclusive[Int]] = None,
	val origin: Option[String] = None,
	val superstyles: Seq[Option[StyleId]] = Seq(),
	val substyles: Seq[Option[StyleId]] = Seq()
) {
	lazy val pageURL="/style/" + styleId
	def id: Option[Id] = Some(styleId)
	lazy val ctime: Option[java.util.Date] = Some(new java.util.Date())
	def descriptiveNameForId: String = name
	// def dupe(id:Id,ctime:java.util.Date): Saveable = {
	// 	copy(styleId=StyleId(Some(id)))
	// }
	
	def toXML = transform(<style/> )
	
	def transform(nodes: scala.xml.NodeSeq): scala.xml.NodeSeq = for (node <- nodes) yield node match {
		case s @ <style>{_*}</style> => { 
			val updates=Attribute("","id",id.get.toString, Null) ++
				Attribute("","name",  name, Null)
			val rangeUpdates=(
				(abv,"ABV") ::
				(ibu,"IBU") :: 
				(og,"OG") :: 
				(fg,"FG") :: 
				(srm,"SRM") :: 
			 Nil).foldLeft(updates) { (r, pair) => r ++ (pair._1 match {
					case Some(v) => Attribute("",pair._2+"lo", v.start.toString, Attribute("",pair._2+"hi", v.end.toString,Null))
					case None => Null
			})}

			rangeUpdates.foldLeft(s.asInstanceOf[Elem])( _ % _ )
		}
		case other => other
	}

	def toJSON = JsObject((
		"id" -> JsString(styleId.toString) ::
		"name" -> JsString(name) ::
		Nil
	))
}

object BeerStyle {
	def getObjects(list: List[StyleId]): List[BeerStyle] = list.map( id => BeerStyle.fromExisting(id) ).filter(_.isDefined).map(_.get.asInstanceOf[BeerStyle])
	
	lazy private val beerStylesXML=scala.xml.XML.loadFile(BeerCrush.datadir + "/beerstyles.xml")

	import SuperNode._
	def fromExisting(id: StyleId): Option[BeerStyle] = id match {
		case StyleId.Undefined => Some(BeerStyle(
			styleId=StyleId.Undefined
			,name="Beer"
			,substyles  =beerStylesXML.child.map(_.attribute("id").map(n => StyleId(n.text)))
		))
		case styleId: StyleId => {
			beerStylesXML \\ "style" find { _.attribute("id").getOrElse("") .toString == id.toString } match { 
				case None => None
				case Some(node) => node.attribute("id") match {
					case None => None
					case Some(id) => Some(BeerStyle(
						styleId=StyleId(id.text)
						,name=node.attribute("name").get.text
						,abv= (node.attribute("ABVlo"), node.attribute("ABVhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble, hi.head.text.toDouble, 0.01))
							case _ => None
						}
						,ibu=(node.attribute("IBUlo"), node.attribute("IBUhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Int.inclusive(lo.head.text.toInt,hi.head.text.toInt,1))
							case _ => None
						}
						,og= (node.attribute("OGlo"),  node.attribute("OGhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble,hi.head.text.toDouble, 0.001))
							case _ => None
						}
						,fg= (node.attribute("FGlo"),  node.attribute("FGhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Double.inclusive(lo.head.text.toDouble, hi.head.text.toDouble, 0.001))
							case _ => None
						}
						,srm= (node.attribute("SRMlo"), node.attribute("SRMhi")) match {
							case (Some(lo),Some(hi)) => Some(Range.Int.inclusive(lo.head.text.toInt, hi.head.text.toInt, 1))
							case _ => None
						}
						,origin=node.attribute("origin").map(_.text)
						,superstyles=node.getAncestors(beerStylesXML).map(_.attribute("id").map(n => StyleId(n.text)))
						,substyles  =node.child.map(_.attribute("id").map(n => StyleId(n.text)))
					))
				}
			}
		}
	}
}
