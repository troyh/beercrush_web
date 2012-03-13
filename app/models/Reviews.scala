package models

import BeerCrush._
import play.api.libs.json._
import play.api._

case class ReviewId(reviewid: String) extends Id(Some(reviewid)) {
	def setUser(username: String) = {
		this.copy(reviewid=id.get.split("/") match {
			case Array(brewery,beer,_,_*) => brewery + "/" + beer + "/review/" + username
		}
		)
	}
	def isComplete: Boolean = id.isDefined && id.get.split("/").length==4
}

object ReviewId {
	def fromBeerId(beerId: BeerId): ReviewId = new ReviewId(beerId.toString + "/review")
	def beerIdFromReviewId(reviewId: ReviewId): BeerId = new BeerId(reviewId.id.get.split("/").dropRight(2).mkString("/"))
	def userIdFromReviewId(reviewId: ReviewId): UserId = new UserId(reviewId.id.get.split("/").last)
	implicit def string2id(s: String): ReviewId = new ReviewId(s)
	implicit def string2oid(id: String): Option[ReviewId] = Some(new ReviewId(id))
	implicit def id2string(id: ReviewId): String = id.id.get
}

abstract class Review {
}

case class BeerReview(
	val id:Option[ReviewId],
	val ctime: Option[java.util.Date],
	val rating:Int,
	val balance: Option[Int],
	val aftertaste: Option[Int],
	val flavors: Option[List[String]],
	val when: Option[java.util.Date],
	val where: Option[String],
	val wouldDrinkAgain: Option[Boolean],
	val text:Option[String]
) extends Review with XmlFormat with JsonFormat with Storage.Saveable {

	lazy val descriptiveNameForId = ReviewId.userIdFromReviewId(id.get).toString

	def dupe(id:Id,ctime:java.util.Date) = {
		this.copy(id=Some(ReviewId(id)),ctime=Some(ctime))
	}

	import scala.xml._
	
	def asXML: xml.Node = 
		<review id={id.get}>
			{ ctime.map { t => <ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(t)}</ctime> }.getOrElse() }
			{ <rating/> % Attribute("","value",rating.toString,Null) }
			{ balance.map { n => <balance/> % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ aftertaste.map { n => <aftertaste/>  % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ when.map { n => <when>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(n)}</when> }.getOrElse() }
			{ where.map { n => <where>{n.toString}</where> }.getOrElse() }
			{ wouldDrinkAgain.map { b => <wouldDrinkAgain/> % Attribute("","value",b.toString,Null) }.getOrElse() }
			{ text.map { s => <text>{s}</text> }.getOrElse() }
		</review>
	def transform(nodes: NodeSeq, xpath: Seq[String] = Seq()): NodeSeq = <review/> // TODO: implement this
		
	def asJson = JsObject(List(
		id.map { x => "id" -> JsString(x.id.get) }.get,
		"ctime" -> JsString(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(ctime)),
		"rating" -> JsNumber(rating),
		balance.map{ "balance" -> JsNumber(_) }.get,
		aftertaste.map{ "aftertaste" -> JsNumber(_) }.get,
		flavors.map{ list => "flavors" -> JsObject(
			list.map { "flavor" -> JsString(_) }
		)}.get,
		wouldDrinkAgain.map{ "wouldDrinkAgain" -> JsBoolean(_) }.get,
		text.map{ "text" -> JsString(_) }.get
	))
	
	lazy val maybeBeer: Option[Beer] = Beer.fromExisting(ReviewId.beerIdFromReviewId(id.get))
	lazy val maybeUser: Option[User] = User.findUser(ReviewId.userIdFromReviewId(id.get))
}

object BeerReview {
	
	def fromExisting(reviewId: ReviewId): Option[BeerReview] = {
		try {
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(reviewId))
			
			Some(BeerReview(
				id = Some(xml \ "@id").headOption.map{s=>Some(ReviewId(s.text))}.get,
				ctime = (xml \ "ctime").headOption.map{s=>Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text))}.getOrElse(None),
				rating = (xml \ "rating" \ "@value").headOption.map{_.text.toInt}.getOrElse(0),
				balance = (xml \ "balance" \ "@value").headOption.map{s => Some(s.text.toInt)}.getOrElse(None),
				aftertaste = (xml \ "aftertaste" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				flavors = (xml \ "flavors" \ "flavor").map{s=>s.text}.toList match {case x if (x.isEmpty) => None; case x => Some(x)},
				when = (xml \ "when").headOption.map{ s => Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text)) }.getOrElse(None),
				where= (xml \ "where").headOption.map{ s => Some(s.text) }.getOrElse(None),
				wouldDrinkAgain = (xml \ "wouldDrinkAgain" \ "@value").headOption.map{s=>Some(s.text.toBoolean)}.getOrElse(None),
				text = (xml \ "text").headOption.map{s=>Some(s.text)}.getOrElse(None)
			))
		}
		catch {
			case _ => None
		}
	}
}
