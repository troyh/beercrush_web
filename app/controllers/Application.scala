package controllers

import play.api._
import play.api.mvc._
import org.apache.solr._
import scalaj.collection.Imports._


class BeerCrushPersistentObject(id:String) {
	val docType=this match {
		case Brewery(id) => "brewery"
		case Beer(id) => "beer"
		case _ => "unknown"
	}
	val xmlFileLocation="/Users/troy/beerdata/" + docType + "/" + id.replace(":","/") + ".xml"
	val asXML=scala.xml.XML.loadFile(xmlFileLocation)
	val name=(asXML \ "name").text
	val pageURL = {
		this match {
			case Brewery(id) => "/" + id
			case Beer(id) => "/" + id
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
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(beer => Beer(beer.get("id").toString))
	}
}

case class Beer(id:String) extends BeerCrushPersistentObject(id) {
	def description: String = (this.asXML \ "description").text
	def abv: String = (this.asXML \ "abv").text
	def ibu: String = (this.asXML \ "ibu").text
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
	  Ok(views.html.beer(Beer(breweryId + "/" + beer),Brewery(breweryId)))
  }

  def showBrewery(brewery:String) = Action {
	  Ok(views.html.brewery(Brewery(brewery)))
  }
  
  def allBreweries(letter:String="", page: Long) = Action {
	  val MAX_ROWS=20
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q","doctype:brewery AND nameForSorting:" + letter.toLowerCase + "*")
	  parameters.setStart(((page-1) * MAX_ROWS).toInt)
	  parameters.setRows(MAX_ROWS)
	  val response=solr.query(parameters)
	  val numFound=response.getResults().getNumFound()
	  val docs=response.getResults().asScala
	  Ok(views.html.allBreweries(docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>),
	  	numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
		page))
  }
  
  def allBeers(letter:String="", page: Long) = Action {
	  val MAX_ROWS=20
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q","doctype:beer AND nameForSorting:" + letter.toLowerCase + "*")
	  parameters.setStart(((page-1) * MAX_ROWS).toInt)
	  parameters.setRows(MAX_ROWS)
	  val response=solr.query(parameters)
	  val numFound=response.getResults().getNumFound()
	  val docs=response.getResults().asScala
	  Ok(views.html.allBreweries(docs.map(d => <beer><id>{d.get("id")}</id><name>{d.get("name")}</name></beer>),
	  	numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
		page))
  }
  
  def search(query:String, page: Long) = Action {
	  val MAX_ROWS=20
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q",query)
	  parameters.set("defType","dismax")
	  parameters.set("qf","name")
	  parameters.setStart(((page-1) * MAX_ROWS).toInt)
	  parameters.setRows(MAX_ROWS)
	  val response=solr.query(parameters)
	  val numFound=response.getResults().getNumFound()
	  val docs=response.getResults().asScala
	  Ok(views.html.search(
		  query,
		  (numFound + (MAX_ROWS-1)) / MAX_ROWS,
		  page,
		  docs
	  	))
  }
  
}