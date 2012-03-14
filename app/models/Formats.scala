package BeerCrush

import play.api.libs.json._

trait JsonFormat {
	def asJson: JsObject
}

trait XmlFormat {
	def asXML: xml.NodeSeq
}

