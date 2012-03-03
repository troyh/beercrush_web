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
			case Some(theid) => (review.copy(id=id),theid)
		}
		
		/* Make the necessary directories to store the review */
		fileLocation(saveId).split("/").drop(datadir_parts.length).foldLeft(datadir){ (path,item) => 
			Logger.info("mkdir'ing " + path + ", item=" + item)
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
			case Array(brewery,beer,review,_*) => brewery + "/" + beer + "/" + review + "/" + username
		}
		)
	}
	def isComplete: Boolean = id.isDefined && id.get.split("/").length==4
}

object ReviewId {
	def fromBeerId(beerId: BeerId): ReviewId = new ReviewId(beerId.toString + "/review")
	implicit def string2id(s: String): ReviewId = new ReviewId(s)
	implicit def id2string(id: ReviewId): String = id.id.get
}

abstract class Review {
}

case class BeerReview(
	val id:Option[ReviewId],
	val rating:Int,
	val bitterness:Option[Int],
	val sweetness:Option[Int],
	val aroma:Option[Int],
	val color:Option[Int],
	val wouldDrinkAgain: Option[Boolean],
	val text:Option[String]
) extends Review with XmlFormat with JsonFormat {

	import scala.xml._
	
	def asXML: xml.Node = 
		<review id={id.get}>
			{ <rating/> % Attribute("","value",rating.toString,Null) }
			{ bitterness.map { n => <bitterness/> % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ sweetness.map { n => <sweetness/>  % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ aroma.map { n => <aroma/> % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ color.map { n => <color/> % Attribute("","value",n.toString,Null) }.getOrElse() }
			{ wouldDrinkAgain.map { b => <wouldDrinkAgain/> % Attribute("","value",b.toString,Null) }.getOrElse() }
			{ text.map { s => <text>{s}</text> }.getOrElse() }
		</review>
		
	def asJson = JsObject(List(
		id.map { x => "id" -> JsString(x.id.get) }.get,
		"rating" -> JsNumber(rating),
		bitterness.map{ "bitterness" -> JsNumber(_) }.get,
		sweetness.map{ "sweetness" -> JsNumber(_) }.get,
		aroma.map{ "aroma" -> JsNumber(_) }.get,
		color.map{ "color" -> JsNumber(_) }.get,
		wouldDrinkAgain.map{ "wouldDrinkAgain" -> JsBoolean(_) }.get,
		text.map{ "text" -> JsString(_) }.get
	))
}

object BeerReview {
	def fromExisting(reviewId: ReviewId): Option[BeerReview] = {
		try {
			Logger.info("Review file loc:" + Storage.fileLocation(reviewId))
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(reviewId))
			Logger.info("Review XML:" + xml.toString)
			
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
				rating = (xml \ "rating" \ "@value").headOption.map{_.text.toInt}.getOrElse(0),
				bitterness = (xml \ "bitterness" \ "@value").headOption.map{s => Some(s.text.toInt)}.getOrElse(None),
				sweetness = (xml \ "sweetness" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				aroma = (xml \ "aroma" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				color = (xml \ "color" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				wouldDrinkAgain = (xml \ "wouldDrinkAgain" \ "@value").headOption.map{s=>Some(s.text.toBoolean)}.getOrElse(None),
				text = (xml \ "text").headOption.map{s=>Some(s.text)}.getOrElse(None)
			))
		}
		catch {
			case _ => {
			Logger.info("Exception in BeerReview")
				None}
		}
	}
}
