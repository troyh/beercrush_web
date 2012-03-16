package models

import BeerCrush._
import play.api.libs.json._
import play.api._
import scala.xml.{NodeSeq, Node, Attribute, Null}

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

	def toXML=transform(<review/>) 

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes,
		Map(
			(BeerReview.xmlTagReview, { orig =>
				<review>{
					applyValuesToXML(orig.child,Map(
						( BeerReview.xmlTagCtime,      { orig => if (ctime.isDefined) <ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(ctime.get)}</ctime> else orig } )
						,(BeerReview.xmlTagRating,     { orig => <rating/> % Attribute("","value",rating.toString,Null) } )
						,(BeerReview.xmlTagBalance,    { orig => if (balance.isDefined) <balance/> % Attribute("","value",balance.get.toString,Null) else orig } )
						,(BeerReview.xmlTagAftertaste, { orig => if (aftertaste.isDefined) <aftertaste/>  % Attribute("","value",aftertaste.get.toString,Null) else orig } )
						,(BeerReview.xmlTagWhen,       { orig => if (when.isDefined) <when>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(when.get)}</when> else orig } )
						,(BeerReview.xmlTagWhere,      { orig => if (where.isDefined) <where>{where.get.toString}</where> else orig } )
						,(BeerReview.xmlTagDrinkAgain, { orig => if (wouldDrinkAgain.isDefined) <wouldDrinkAgain/> % Attribute("","value",wouldDrinkAgain.get.toString,Null) else orig } )
						,(BeerReview.xmlTagText,       { orig => if (text.isDefined) <text>{text}</text> else orig } )
					))
				}</review>
			})
		)
	)
		
	def toJSON = JsObject(List(
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

	private final val xmlTagReview	   = "review"
	private final val xmlTagId		   = "id"
	private final val xmlAttribId	   = "@" + xmlTagId
	private final val xmlTagCtime	   = "ctime"
	private final val xmlTagRating	   = "rating"
	private final val xmlTagBalance	   = "balance"
	private final val xmlTagAftertaste = "aftertaste"
	private final val xmlTagWhen	   = "when"
	private final val xmlTagWhere	   = "where"
	private final val xmlTagDrinkAgain = "wouldDrinkAgain"
	private final val xmlTagText	   = "text"
	private final val xmlTagFlavors	   = "flavors"
	private final val xmlTagFlavor	   = "flavor"
	private final val xmlTagValue	   = "value"
	private final val xmlAttribValue   = "@" + xmlTagValue

	def fromExisting(reviewId: ReviewId): Option[BeerReview] = {
		try {
			val xml=scala.xml.XML.loadFile(Storage.fileLocation(reviewId))
			
			Some(BeerReview(
				id = Some(	  xml \ xmlAttribId					     ).headOption.map{s=>Some(ReviewId(s.text))}.get,
				ctime =  	 (xml \ xmlTagCtime					     ).headOption.map{s=>Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text))}.getOrElse(None),
				rating = 	 (xml \ xmlTagRating \ xmlAttribValue    ).headOption.map{_.text.toInt}.getOrElse(0),
				balance = 	 (xml \ xmlTagBalance \ xmlAttribValue   ).headOption.map{s => Some(s.text.toInt)}.getOrElse(None),
				aftertaste = (xml \ xmlTagAftertaste \ xmlAttribValue).headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				flavors = 	 (xml \ xmlTagFlavors \ xmlTagFlavor     ).map{s=>s.text}.toList match {case x if (x.isEmpty) => None; case x => Some(x)},
				when = 		 (xml \ xmlTagWhen						 ).headOption.map{ s => Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text)) }.getOrElse(None),
				where= 		 (xml \ xmlTagWhere						 ).headOption.map{ s => Some(s.text) }.getOrElse(None),
				wouldDrinkAgain = (xml \ xmlTagDrinkAgain \ xmlAttribValue).headOption.map{s=>Some(s.text.toBoolean)}.getOrElse(None),
				text = 		 (xml \ xmlTagText                       ).headOption.map{s=>Some(s.text)}.getOrElse(None)
			))
		}
		catch {
			case _ => None
		}
	}
}
