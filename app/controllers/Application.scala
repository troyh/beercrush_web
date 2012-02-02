package controllers

import play.api._
import play.api.mvc._
import org.apache.solr._
import scalaj.collection.Imports._


class BeerCrushPersistentObject(id:String) {
	val docType=this match {
		case Brewery(id) => "brewery"
		case Beer(id,breweryId) => "beer"
	}
	val xmlFileLocation="/Users/troy/beerdata/" + docType + "/" + id.replace(":","/") + ".xml"
	val asXML=scala.xml.XML.loadFile(xmlFileLocation)
	val name=(asXML \ "name").text
	val pageURL = {
		this match {
			case Brewery(id) => "/brewery/" + id
			case _ => "/"
		}
	}
}

case class Brewery(id:String) extends BeerCrushPersistentObject(id) {
	val address={
		val address=this.asXML \ "address"
		new {
			val street =(address \ "street").text
			val city   =(address \ "city").text
			val state  =(address \ "state").text
			val zip    =(address \ "zip").text
			val country=(address \ "country").text
		}
	}
	val phone=(this.asXML \ "phone").text
}

case class Beer(id:String,val breweryId:String) extends BeerCrushPersistentObject(breweryId + ":" + id) {
	def description: String = (this.asXML \ "description").text
	def abv: Float = (this.asXML \ "abv").text.toFloat
	def ibu: Int = (this.asXML \ "ibu").text.toInt
	def styles: Seq[BeerStyle] = {
		(this.asXML \ "styles").map( style => new BeerStyle((style \ "style" \ "bjcp_style_id").text,(style \ "style" \ "name").text))
	}
}

class BeerStyle(id: String, val name: String) {
	val pageURL="/style/" + id
}

object Application extends Controller {

  val solr=new org.apache.solr.client.solrj.impl.CommonsHttpSolrServer("http://localhost:8983/solr")
  
  
  def index = Action {
    Ok(views.html.index("Beer Crush"))
  }
  
  def showBeer(breweryId:String,beer:String) = Action {
	  // Beer(brewery,beer)
	  val beerDoc=xml.XML.loadFile("/Users/troy/beerdata/beer/" + breweryId + "/" + beer + ".xml")
	  Ok(views.html.beer(Beer(beer,breweryId),Brewery(breweryId)))
  }

  def showBrewery(brewery:String) = Action {
	  Ok(views.html.brewery(Brewery(brewery)))
  }
  
  def allBreweries() = Action {
	  // val response=SolrQuery("*")
	  
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q","*")
	  val response=solr.query(parameters)
	  val docs=response.getResults().asScala
	  Ok(views.html.allBreweries(docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>)))
  }
  
}