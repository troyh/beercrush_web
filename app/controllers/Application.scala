package controllers

import util.parsing.combinator._
import play.api._
import play.api.mvc._
import org.apache.solr._
import scalaj.collection.Imports._

// object Global extends GlobalSettings {
// 	val config = new {
// 		val solr = new {
// 			var url: String = ""
// 		}
// 		val storage = new {
// 			var location:String = "/tmp"
// 		}
// 	}
// 	override def onStart(app:Application) {
// 		this.config.solr.url=app.configuration.getString("beercrush.solr.url").getOrElse("")
// 		this.config.storage.location=app.configuration.getString("beercrush.storage.location").getOrElse("")
// 	}
// }

trait BeerCrushPersistentObject {
	val id:String
	val docType: String = this match {
		case b: Brewery => "brewery"
		case b: Beer => "beer"
	}
	val pageURL: String = "/" + id
}

case class Brewery(
	val id:		String,
	val name: 	String,
	val address: Address,
	val phone:	String
) extends BeerCrushPersistentObject {
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(doc => Beer.fromExisting(doc.get("id").toString))
	}
}

class Address(
	val street			: String,
	val city			: String,
	val state			: String,
	val zip				: String,
	val country			: String
)

object Brewery {
	def fromExisting(id:String) = {
		val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/brewery/" + id + ".xml")
		val address=xml \ "address"
		new Brewery(
			(xml \ "id").text,
			(xml \ "name").text,
			new Address(
				(address \ "street").text,
				(address \ "city").text,
				(address \ "state").text,
				(address \ "zip").text,
				(address \ "country").text
			),
			(xml \ "phone").text
		)
	}
}

case class Beer(
	val id:			String,
	val breweryId:	String,
	val name: 		String,
	val description:String,
	val abv: 		Double,
	val ibu: 		Int,
	val ingredients:String,
	val grains:		String,
	val hops:		String,
	val yeast:		String,
	val otherings:	String,
	val styles: 	List[BeerStyle]
) extends BeerCrushPersistentObject

object MyHelpers {
	def ifException[T](f: => T)(default:T): T = {
		try {
			f
		} catch { 
			case _ => default
		}
	}
}

object Beer {
	def fromExisting(id:String): Beer = {
		import MyHelpers._
		val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/beer/" + id + ".xml")
		Beer(
			id = id,
			breweryId = (xml \ "brewery_id").text,
			name = (xml \ "name").text,
			description = (xml \ "description").text,
			abv = ifException { (xml \ "abv").text.toDouble } (0.0) ,
			ibu = ifException { (xml \ "ibu").text.toInt } (0),
			ingredients = (xml \ "ingredients").text,
			grains = (xml \ "grains").text,
			hops = (xml \ "hops").text,
			yeast = (xml \ "yeast").text,
			otherings = (xml \ "otherings").text,
			styles = (xml \ "styles").map( style => 
				new BeerStyle((style \ "style" \ "bjcp_style_id").text,(style \ "style" \ "name").text)
			).toList
		)
	}
	def store(beer: Beer): Beer = {
		val xml=
		<beer>
		  <id>{beer.id}</id>
		  <brewery_id>{beer.breweryId}</brewery_id>
		  <calories_per_ml></calories_per_ml>
		  <abv>{beer.abv}</abv>
		  <ibu>{beer.ibu}</ibu>
		  <name>{beer.name}</name>
		  <description>{beer.description}</description>
		  <availability></availability>
		  <ingredients>{beer.ingredients}</ingredients>
		  <grains>{beer.grains}</grains>
		  <hops>{beer.hops}</hops>
		  <yeast>{beer.yeast}</yeast>
		  <otherings>{beer.otherings}</otherings>
		  <styles>
		  			  {beer.styles.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>)}
		  </styles>
		</beer>
		
		scala.xml.XML.loadString(xml.toString)
		scala.xml.XML.save("/Users/troy/beerdata/editedBeer.xml",xml,"UTF-8",true)

		beer
	}
}

class BeerStyle(val id: String, val name: String) {
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

	import play.api.data._
	import play.api.data.Forms._
	import play.api.data.format.Formats._
	import play.api.data.format._
	import play.api.data.validation.Constraints._

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

  val beerForm: Form[Beer] = Form(
	  mapping(
	  		"id" -> text,
			"breweryId" -> nonEmptyText,
			"name" -> nonEmptyText,
			"description" -> nonEmptyText(minLength=10),
			"abv" -> text, // No float or double types?!
			"ibu" -> number(min=0,max=200),
	  		"ingredients" -> text,
			"grains" -> text,
			"hops" -> text,
			"yeast" -> text,
			"otherings" -> text,
			"styles" -> list(text)
	  )
	  {
  		  (id:String,breweryId:String,name:String,description:String,abv:String,ibu:Int,ingredients:String,grains:String,hops:String,yeast:String,otherings:String,styles:List[String]) => {
  			  Beer(id,
				  breweryId,
				  name,
				  description,
				  abv.toDouble,
				  ibu,
				  ingredients,
				  grains,
				  hops,
				  yeast,
				  otherings,
				  styles.map(s => new BeerStyle(s,s))
			  )
  		  }
	  }
	  {
		  beer => {
			  Some(beer.id,
				  beer.breweryId,
				  beer.name,
				  beer.description,
				  beer.abv.toString,
				  beer.ibu,
				  beer.ingredients,
				  beer.grains,
				  beer.hops,
				  beer.yeast,
				  beer.otherings,
				  beer.styles.map(s => s.id)
			  )
		  }
	  }
  )
	  
  def showBeer(breweryId:String,beerId:String) = Action { request => 
	  val beer=Beer.fromExisting(breweryId + "/" + beerId)
	  val brewery=Brewery.fromExisting(breweryId)
	  
	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		  case AcceptHTMLHeader => Ok(views.html.beer(beer,brewery,beerForm.fill(beer)))
		  case AcceptXMLHeader  => Ok(views.xml.beer(beer,brewery))
	  }
  }

  def showBrewery(breweryId:String) = Action { request =>
	  val brewery=Brewery.fromExisting(breweryId)

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
  
  def search(query:String, page: Long) = Action { request =>
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
  	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
  		case AcceptHTMLHeader => Ok(views.html.search(
			query,
			(numFound + (MAX_ROWS-1)) / MAX_ROWS,
			page,
			docs))
  		case AcceptXMLHeader  => Ok(views.xml.search(
			query,
			numFound,
			response.getResults().getStart(),
			docs))
  	  }
  }
  
  def editBeer(breweryId:String, beerId:String) = Action { implicit request => 
	  beerForm.bindFromRequest.fold(
		  // Handle errors
		  errors => {
			  BadRequest(errors.toString)
			  // Ok(views.html.beer(Beer(breweryId + "/" + beerId),Brewery(breweryId),beerForm))
			  
		  },
	      // Handle successful form submission
	      beer => {
			  // beer.id=breweryId + "/" + beerId
			  // beer.breweryId=breweryId
			  // Save the doc
			  Beer.store(beer)
			  val brewery=Brewery.fromExisting(beer.breweryId)
	  
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.beer(beer,brewery,beerForm))
				  case AcceptXMLHeader  => Ok(views.xml.beer(beer,brewery))
			  }
		  }
	  )
  }
  
}