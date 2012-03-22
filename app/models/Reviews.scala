package models

import BeerCrush._
import play.api.libs.json._
import play.api._
import scala.xml.{NodeSeq, Node, Attribute, Null, Elem, Text}

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
	
	import SuperNode._
	def transform(nodes: NodeSeq): NodeSeq = for (node <- nodes) yield node match {
		case r @ <review>{_*}</review> => r.asInstanceOf[Elem] % 
				Attribute("","id",id.getOrElse("").toString,Null) %
				Attribute("","ctime",Utility.formatDateISO8601(ctime),Null) copy(child=for (k <- r.withMissingChildElements(Seq("rating","balance","aftertaste","flavors","when","where","wouldDrinkAgain","text")).child) yield k match {
			case <ctime>{_*}</ctime> => <ctime/> // Deletes it on Storage.save()
			case <rating>{_*}</rating> => k.asInstanceOf[Elem] % Attribute("","value",rating.toString,Null)
			case <balance>{_*}</balance> => 
				if (balance.isDefined) 
					k.asInstanceOf[Elem] % Attribute("","value",balance.get.toString,Null) 
				else 
					k.asInstanceOf[Elem].copy(attributes=k.attributes.remove("value"))
			case <aftertaste>{_*}</aftertaste> => 
				if (aftertaste.isDefined) 
					k.asInstanceOf[Elem] % Attribute("","value",aftertaste.get.toString,Null) 
				else
					k.asInstanceOf[Elem].copy(attributes=k.attributes.remove("value"))
			case <flavors>{_*}</flavors> => 
				if (flavors.isDefined)
					k.asInstanceOf[Elem].copy(child=flavors.get.map { s => <flavor>{s}</flavor> })
				else
					k.asInstanceOf[Elem].copy(child=Text(""))
			case <when>{_*}</when> => 
				if (when.isDefined)
					k.asInstanceOf[Elem].copy(child=Text(Utility.formatDateISO8601(when)))
				else
					k.asInstanceOf[Elem].copy(child=Text(""))
			case <where>{_*}</where> => k.asInstanceOf[Elem].copy(child=Text(where.get))
			case <wouldDrinkAgain>{_*}</wouldDrinkAgain> => 
				if (wouldDrinkAgain.isDefined) 
					k.asInstanceOf[Elem] % Attribute("","value",wouldDrinkAgain.get.toString,Null) 
				else 
					k.asInstanceOf[Elem].copy(attributes=k.attributes.remove("value"))
			case <text>{_*}</text> => k.asInstanceOf[Elem].copy(child=Text(text.getOrElse("")))
			case other => other
		})
		case other => other
	}
	
	def toJSON = JsObject(List(
		id.map { x => "id" -> JsString(x.id.get) }.get,
		"ctime" -> JsString(Utility.formatDateISO8601(ctime)),
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
				id = Some(	  xml \ "@id"				   ).headOption.map{s=>Some(ReviewId(s.text))}.get,
				ctime =  	 (xml \ "ctime"				   ).headOption.map{s=>Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text))}.getOrElse(None),
				rating = 	 (xml \ "rating" \ "@value"    ).headOption.map{_.text.toInt}.getOrElse(0),
				balance = 	 (xml \ "balance" \ "@value"   ).headOption.map{s => Some(s.text.toInt)}.getOrElse(None),
				aftertaste = (xml \ "aftertaste" \ "@value").headOption.map{s=>Some(s.text.toInt)}.getOrElse(None),
				flavors = 	 (xml \ "flavors" \ "flavor"   ).map{s=>s.text}.toList match {case x if (x.isEmpty) => None; case x => Some(x)},
				when = 		 (xml \ "when"				   ).headOption.map{ s => Some(new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).parse(s.text)) }.getOrElse(None),
				where= 		 (xml \ "where"				   ).headOption.map{ s => Some(s.text) }.getOrElse(None),
				wouldDrinkAgain = (xml \ "wouldDrinkAgain" \ "@value").headOption.map{s=>Some(s.text.toBoolean)}.getOrElse(None),
				text = 		 (xml \ "text"                 ).headOption.map{s=>Some(s.text)}.getOrElse(None)
			))
		}
		catch {
			case _ => None
		}
	}
}
