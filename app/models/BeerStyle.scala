package models

import BeerCrush._
import Storage._
import scala.collection.immutable.{Range, NumericRange}
import scala.annotation._
import play.api.libs.json._

case class StyleId(styleId: Option[String]) extends Id(styleId) {
}

object StyleId {
	implicit def string2StyleId(s: String) = new StyleId(Some(s))
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
) extends Saveable {
	lazy val pageURL="/style/" + styleId
	def id: Option[Id] = Some(styleId)
	lazy val ctime: Option[java.util.Date] = Some(new java.util.Date())
	def descriptiveNameForId: String = name
	def dupe(id:Id,ctime:java.util.Date): Saveable = {
		copy(styleId=StyleId(Some(id)))
	}
	
	def toXML = Storage.transform(this, <style/> )
	def transform(nodes: scala.xml.NodeSeq): scala.xml.NodeSeq = Storage.transform(this, nodes)
	def toJSON = JsObject((
		"id" -> JsString(styleId) ::
		"name" -> JsString(name) ::
		Nil
	))
}

object BeerStyle {
	def getObjects(list: List[StyleId]): List[BeerStyle] = list.map( id => Storage.load(id) ).filter(_.isDefined).map(_.get.asInstanceOf[BeerStyle])
}
