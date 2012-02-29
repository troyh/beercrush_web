package models

import BeerCrush._
import play.api.libs.json._

class ReviewId(id: String) extends Id(Some(id)) {
}

object ReviewId {
	def fromBeerId(beerId: BeerId): ReviewId = new ReviewId(beerId.toString)
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
	def save = scala.xml.XML.save("/Users/troy/beerdata/reviews/" + id + ".xml",asXML,"UTF-8",true)
	
	def asXML: xml.Node = 
		<review>
			<id>{id}</id>
			<rating>{rating}</rating>
			{ bitterness.map { n => <bitterness>n</bitterness> } }
			{ sweetness.map { n => <sweetness>n</sweetness> } }
			{ aroma.map { n => <aroma>n</aroma> } }
			{ color.map { n => <color>n</color> } }
			{ wouldDrinkAgain.map { b => <wouldDrinkAgain>b</wouldDrinkAgain> } }
			{ text.map { s => <text>s</text> } }
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
			val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/reviews/" + reviewId + ".xml")
			
			xml match {
				case <review>{ d @ _* }</review> => {
					Some(d.foldLeft(BeerReview(None,0,None,None,None,None,None,None)) { (review,item) => item match {
						case <rating>{r}</rating> => review.copy(rating=r.text.toInt)
						case <bitterness>{b}</bitterness> => review.copy(bitterness=Some(b.text.toInt))
					}})
				}
				case _ => None
			}
			
			// Some(BeerReview(
			// 	reviewId = Some(reviewId),
			// 	rating = (xml \ "rating").headOption.map{_.text.toInt}.getOrElse(0),
			// 	bitterness = (xml \ "bitterness").headOption.map{_.text.toInt},
			// 	sweetness = (xml \ "sweetness").headOption.map{_.text.toInt},
			// 	aroma = (xml \ "aroma").headOption.map{_.text.toInt},
			// 	color = (xml \ "color").headOption.map{_.text.toInt},
			// 	wouldDrinkAgain = (xml \ "wouldDrinkAgain").headOption.map{_.text.toBoolean},
			// 	text = (xml \ "text").headOption.map{_.text}
			// ))
		}
		catch {
			case _ => None
		}
	}
}
