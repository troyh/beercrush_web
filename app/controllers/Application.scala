package controllers

import util.parsing.combinator._
import play.api._
import play.api.mvc._
import org.apache.solr._
import scalaj.collection.Imports._


class BeerCrushPersistentObject(id:String) {
	lazy val docType=this match {
		case Brewery(id) => "brewery"
		case Beer(id) => "beer"
		case _ => "unknown"
	}
	lazy val xmlFileLocation="/Users/troy/beerdata/" + docType + "/" + id.replace(":","/") + ".xml"
	lazy val asXML=scala.xml.XML.loadFile(xmlFileLocation)
	lazy val name=(asXML \ "name").text
	lazy val pageURL = {
		this match {
			case Brewery(id) => "/" + id
			case Beer(id) => "/" + id
			case _ => "/"
		}
	}
}

case class Brewery(id:String) extends BeerCrushPersistentObject(id) {
	lazy val address={
		val address=this.asXML \ "address"
		new {
			val street =(address \ "street").text
			val city   =(address \ "city").text
			val state  =(address \ "state").text
			val zip    =(address \ "zip").text
			val country=(address \ "country").text
		}
	}
	lazy val phone=(this.asXML \ "phone").text
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
	lazy val pageURL="/style/" + id
}

// This parsing code from http://www.suryasuravarapu.com/2011/04/scala-parser-combinators-win.html
case class AcceptHeader(mediaType: String, mediaSubType: String, qualityFactor: Float)

object AcceptHeaderParser extends JavaTokenParsers {
  lazy val accept: Parser[List[AcceptHeader]] = rep1sep(acceptEntry, ",")
  lazy val acceptEntry: Parser[AcceptHeader] = (mediaType <~ "/") ~ mediaSubType ~ opt(qualityFactor) ^^ {
    case t ~ st ~ Some(q) => AcceptHeader(t, st, q.toFloat)
    case t ~ st ~ None => AcceptHeader(t, st, 1.0F)
  }
  lazy val wordRegex = """[\w+\-*]*""".r
  lazy val mediaType = wordRegex
  lazy val mediaSubType = wordRegex
  lazy val qualityFactor = ";" ~> "q" ~> "=" ~> floatingPointNumber

  def parse(input: String): List[AcceptHeader] = parseAll(accept, input).getOrElse(Nil)
}


object Application extends Controller {

  val solr=new org.apache.solr.client.solrj.impl.CommonsHttpSolrServer("http://localhost:8983/solr")

  def index = Action {
    Ok(views.html.index("Beer Crush"))
  }

	sealed abstract class AcceptHeaderType 
	case object AcceptXMLHeader extends AcceptHeaderType
	case object AcceptHTMLHeader extends AcceptHeaderType

  def matchAcceptHeader(ahl: List[AcceptHeader]): AcceptHeaderType = {
	  ahl match {
		  case AcceptHeader("text","html",_) :: rest => AcceptHTMLHeader
		  case AcceptHeader("text","xml",_) :: rest => AcceptXMLHeader
		  case head :: rest => matchAcceptHeader(rest)
		  case Nil => AcceptHTMLHeader
	  }
  }

  def showBeer(breweryId:String,beerId:String) = Action { request => 
	  val beer=Beer(breweryId + "/" + beerId)
	  val brewery=Brewery(breweryId)
	  
	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		  case AcceptHTMLHeader => Ok(views.html.beer(beer,brewery))
		  case AcceptXMLHeader  => Ok(views.xml.beer(beer,brewery))
	  }
  }

  def showBrewery(breweryId:String) = Action { request =>
	  val brewery=Brewery(breweryId)

	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		  case AcceptHTMLHeader => Ok(views.html.brewery(brewery))
		  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery))
	  }
  }
  
  def allBreweries(letter:String="", page: Long) = Action { request =>
	  val MAX_ROWS=20
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q","doctype:brewery AND nameForSorting:" + letter.toLowerCase + "*")
	  parameters.setStart(((page-1) * MAX_ROWS).toInt)
	  parameters.setRows(MAX_ROWS)
	  val response=solr.query(parameters)
	  val numFound=response.getResults().getNumFound()
	  val docs=response.getResults().asScala
  	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
  		  case AcceptHTMLHeader => Ok(views.html.allBreweries(
			docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>),
	  	  	numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
	  		page))
  		  case AcceptXMLHeader  => Ok(views.xml.allBreweries(
  			docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>),
  	  	  	numFound,
  	  		response.getResults().getStart()))
  	  }
  }
  
  def allBeers(letter:String="", page: Long) = Action { request =>
	  val MAX_ROWS=20
	  val parameters=new org.apache.solr.client.solrj.SolrQuery()
	  parameters.set("q","doctype:beer AND nameForSorting:" + letter.toLowerCase + "*")
	  parameters.setStart(((page-1) * MAX_ROWS).toInt)
	  parameters.setRows(MAX_ROWS)
	  val response=solr.query(parameters)
	  val numFound=response.getResults().getNumFound()
	  val docs=response.getResults().asScala
	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		case AcceptHTMLHeader => Ok(views.html.allBeers(
			docs.map(d => <beer><id>{d.get("id")}</id><name>{d.get("name")}</name></beer>),
			numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
			page))
		case AcceptXMLHeader  => Ok(views.xml.allBeers(
			docs.map(d => <beer><id>{d.get("id")}</id><name>{d.get("name")}</name></beer>),
			numFound,
			response.getResults().getStart()))
	  }
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