package models

import BeerCrush._
import play.api.libs.json._
import play.api._

object Storage {
	val datadir="/Users/troy/beerdata"
	lazy val datadir_parts=datadir.split("/")
	def fileLocation(reviewId: ReviewId) = {
		val parts=reviewId.id.get.split("/")
		datadir + "/beer/" + parts.mkString("/") + ".xml"
	}
	
	def save(review: BeerReview, id: Option[ReviewId] = None) = {
		val (reviewToSave,saveId) = id match {
			case None => (review, review.id.get)
			case Some(theid) => (
				review.copy(
					id=id,
					ctime={if (review.ctime.isEmpty) Some(new java.util.Date()) else review.ctime}
				),
				theid)
		}
		
		/* Make the necessary directories to store the review */
		fileLocation(saveId).split("/").drop(datadir_parts.length).foldLeft(datadir){ (path,item) => 
			val f=new java.io.File(path)
			f.mkdir()
			path + "/" + item
		}
		scala.xml.XML.save(fileLocation(saveId),reviewToSave.asXML,"UTF-8",true)
	}
	
}

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
	val details: BeerReview.DrankDetails,
	val wouldDrinkAgain: Option[Boolean],
	val text:Option[String]
) extends Review with XmlFormat with JsonFormat {

	import scala.xml._
	
	def asXML: xml.Node = 
		<review id={id.get}>
			{ ctime.map { t => <ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(t)}</ctime> }.getOrElse() }
			{ <rating/> % Attribute("","value",rating.toString,Null) }
			{ balance.map { n => <balance/> % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ aftertaste.map { n => <aftertaste/>  % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ details.when.map { n => <when>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(n)}</when> }.getOrElse() }
			{ details.where.map { n => <where>{n.toString}</where> }.getOrElse() }
			{ wouldDrinkAgain.map { b => <wouldDrinkAgain/> % Attribute("","value",b.toString,Null) }.getOrElse() }
			{ text.map { s => <text>{s}</text> }.getOrElse() }
		</review>
		
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
	
	class DrankDetails(
		val when: Option[java.util.Date],
		val where: Option[String]
	)
	
	def fromExisting(reviewId: ReviewId): Option[BeerReview] = {
		try {
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(reviewId))
			
			// xml match {
			// 	case <review>{ d @ _* }</review> => {
			// 		Some(d.foldLeft(BeerReview(None,0,None,None,None,None,None,None)) { (review,item) => item match {
			// 			case <rating>{r}</rating> => review.copy(rating=r.text.toInt)
			// 			case <bitterness>{b}</bitterness> => review.copy(bitterness=Some(b.text.toInt))
			// 		}})
			// 	}
			// 	case _ => None
			// }
			
			Some(BeerReview(
				id = Some(xml \ "@id").headOption.map{s=>Some(ReviewId(s.text))}.get,
				ctime = (xml \ "ctime").headOption.map{s=>Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text))}.getOrElse(None),
				rating = (xml \ "rating" \ "@value").headOption.map{_.text.toInt}.getOrElse(0),
				balance = (xml \ "balance" \ "@value").headOption.map{s => Some(s.text.toInt)}.getOrElse(None),
				aftertaste = (xml \ "aftertaste" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				flavors = (xml \ "flavors" \ "flavor").map{s=>s.text}.toList match {case x if (x.isEmpty) => None; case x => Some(x)},
				details = new BeerReview.DrankDetails(
					(xml \ "when").headOption.map{ s => Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text)) }.getOrElse(None)
					,(xml \ "where").headOption.map{ s => Some(s.text) }.getOrElse(None)
				),
				wouldDrinkAgain = (xml \ "wouldDrinkAgain" \ "@value").headOption.map{s=>Some(s.text.toBoolean)}.getOrElse(None),
				text = (xml \ "text").headOption.map{s=>Some(s.text)}.getOrElse(None)
			))
		}
		catch {
			case _ => None
		}
	}
}
