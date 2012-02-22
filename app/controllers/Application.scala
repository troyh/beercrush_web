package controllers

import util.parsing.combinator._
import play.api._
import play.api.mvc._
import play.api.mvc.Security._
import play.api.libs.json._
import org.apache.solr._
import scalaj.collection.Imports._
import play.api.data.validation.{Constraint, Valid, Invalid, ValidationError}
import models._
import java.io._
import BeerCrush._	

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

trait JsonFormat {
	def asJson: JsObject
}



case class Brewery(
	breweryId:	BeerCrush.BreweryId,
	val name: 	String,
	val address: Address,
	val phone:	String
) extends PersistentObject(breweryId) with JsonFormat {
	lazy val pageURL = { "/" + id }
	def beerList: Seq[Beer] = {
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beer AND brewery:" + id)
		val response=Application.solr.query(parameters)
		val docs=response.getResults().asScala
		docs.map(doc => Beer.fromExisting(doc.get("id").toString))
	}

	def save: Unit = {
		val xml=
		<brewery>
		  <id>{this.id}</id>
		  <name>{this.name}‎</name>
		  <address>
		    <street>{this.address.street}</street>
		    <city>{this.address.city}</city>
		    <state>{this.address.state}</state>
		    <zip>{this.address.zip}</zip>
		    <latitude></latitude>
		    <longitude></longitude>
		    <country>{this.address.country}</country>
		  </address>
		  <phone>{this.phone}</phone>
		</brewery>	
		
		scala.xml.XML.loadString(xml.toString)
		scala.xml.XML.save("/Users/troy/beerdata/editedBrewery.xml",xml,"UTF-8",true)
	}
	
	def asJson = {
		JsObject(List(
			"id" -> JsString(this.id.toString),
			"name" -> JsString(this.name),
			"address" -> this.address.asJson,
			"phone" -> JsString(this.phone)
		))
	}
}

case class Address(
	val street			: String,
	val city			: String,
	val state			: String,
	val zip				: String,
	val country			: String
) extends JsonFormat {
	def asJson = {
		JsObject(List(
			"street" -> JsString(this.street),
			"city" -> JsString(this.city),
			"state" -> JsString(this.state),
			"zip" -> JsString(this.zip),
			"country" -> JsString(this.country)
		))
	}
}
object Address {
	def fromXML(node: xml.NodeSeq) = {
		new Address(
			(node \ "street").text,
			(node \ "city").text,
			(node \ "state").text,
			(node \ "zip").text,
			(node \ "country").text
		)
	}
}

object Brewery {
	def fromExisting(id:BeerCrush.BreweryId) = {
		val xml=scala.xml.XML.loadFile("/Users/troy/beerdata/brewery/" + id + ".xml")
		val address=xml \ "address"
		new Brewery(
			(xml \ "id").text,
			(xml \ "name").text,
			Address.fromXML(xml \ "address"),
			(xml \ "phone").text
		)
	}
}

case class Beer(
	beerId:			BeerCrush.BeerId,
	val breweryId:	BeerCrush.BreweryId,
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
) extends BeerCrush.PersistentObject(beerId) with JsonFormat {
	lazy val pageURL = { "/" + id }

	def save = {
		val xml=
		<beer>
		  <id>{this.id}</id>
		  <brewery_id>{this.breweryId}</brewery_id>
		  <calories_per_ml></calories_per_ml>
		  <abv>{this.abv}</abv>
		  <ibu>{this.ibu}</ibu>
		  <name>{this.name}</name>
		  <description>{this.description}</description>
		  <availability></availability>
		  <ingredients>{this.ingredients}</ingredients>
		  <grains>{this.grains}</grains>
		  <hops>{this.hops}</hops>
		  <yeast>{this.yeast}</yeast>
		  <otherings>{this.otherings}</otherings>
		  <styles>
		  			  {this.styles.map(style => <style><bjcp_style_id>{style.id}</bjcp_style_id><name>{style.name}</name></style>)}
		  </styles>
		</beer>
		
		scala.xml.XML.loadString(xml.toString)
		scala.xml.XML.save("/Users/troy/beerdata/editedBeer.xml",xml,"UTF-8",true)
	}
	
	def asJson = {
	  JsObject(List(
		  "id" -> JsString(this.id.toString),
		  "brewery" -> JsString(this.breweryId.toString),
		  "name" -> JsString(this.name),
		  "description" -> JsString(this.description),
		  "styles" -> JsArray(this.styles.map(s => JsObject(List(
			  "id" -> JsString(s.id),
			  "name" -> JsString(s.name)
		  )))),
		  "abv" -> JsNumber(this.abv),
		  "ibu" -> JsNumber(this.ibu)
	  ))
	}
}

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
	def fromExisting(id:BeerCrush.BeerId): Beer = {
		import MyHelpers._
		val xml=scala.xml.XML.loadFile(BeerCrush.PersistentObject.fileLocationFromId(id))
		Beer(
			beerId = id,
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

  def index = Action { implicit request => 
    Ok(views.html.index("Beer Crush"))
  }

	sealed abstract class AcceptHeaderType 
	case object AcceptXMLHeader extends AcceptHeaderType
	case object AcceptJSONHeader extends AcceptHeaderType
	case object AcceptHTMLHeader extends AcceptHeaderType

  def matchAcceptHeader(ahl: List[AcceptHeader]): AcceptHeaderType = {
	  ahl match {
		  case AcceptHeader("text","html",_) :: rest => AcceptHTMLHeader
		  case AcceptHeader("text","xml",_) :: rest => AcceptXMLHeader
		  case AcceptHeader("application","json",_) :: rest => AcceptJSONHeader
		  case head :: rest => matchAcceptHeader(rest)
		  case Nil => AcceptHTMLHeader
	  }
  }

  class LoginForm extends Form[User](
	  mapping(
		  "username" -> nonEmptyText,
		  "password" -> nonEmptyText
	  )
	  { (username,password) => User.findUser(username).get }
	  { user => Some(user.id,user.password)}.verifying( user => {
			  val existingUser=User.findUser(user.id)
			  existingUser.isDefined && user.password==existingUser.get.password
	  }),
	  Map.empty,
	  Nil,
	  None
  )

  class CreateAccountForm extends Form[User](
	  mapping(
		  "username" -> nonEmptyText,
		  "passwords" -> tuple(
			  "password1" -> text(minLength=6),
			  "password2" -> text
		  ).verifying("Passwords don't match", passwords => passwords._1 == passwords._2)
	  )
	  { (username,passwords) => new User(UserId.string2id(username),passwords._1,"") }
	  { user => Some(user.id,(user.password,user.password))}.verifying(
		  "This username is already taken",
		  user => !User.findUser(user.id).isDefined
	  ),
	  Map.empty,
	  Nil,
	  None
  )
	  
  class UserForm(username: UserId) extends Form[User](
	  mapping(
		  "password" -> tuple(
			  "main" -> text,
			  "confirm" -> text
		  ).verifying("Passwords don't match", passwords => passwords._1 == passwords._2),
		  "name" -> text
	  )
	  { (password,name) => new User(username,password._1,name) }
	  { user => Some(("",""),user.name)},
	  Map.empty,
	  Nil,
	  None
  )
  
  class BeerForm(breweryId:BeerCrush.BreweryId, beerId: BeerCrush.BeerId) extends Form[Beer](
	  mapping(
			"name" -> nonEmptyText,
			"description" -> text,
			"abv" -> text.transform({ s: String =>
				/* Extract the digits from any noise, i.e., percent sign (%) or leading spaces */
					val regex="^\\s*([\\d\\.]+)\\s*%?\\s*$".r
					s match {
						case regex(digits) => digits
						case _ => s
					}
				},{ s: String => s }).verifying(
				/**
				*	The ABV value must be numeric (including real numbers), convertable to a Double
				*	and between 0 and 100. The string can have a percentage sign (%) that gets ignored.
				*/
				Constraint { abv: String => {
					abv.isEmpty() match {
						case true => Valid
						case false => try {
							val regex="^\\s*([\\d\\.]+)\\s*%?\\s*$".r
							abv match {
								case regex(digits) => {
									val value=digits.toDouble
									(0 <= value && value <= 25) match {
										case true => Valid
										case false => Invalid(ValidationError("Must be between 0 and 25"))
									}
								}
								case _ => Invalid(ValidationError("Must be a percentage"))
							}
						}
						catch {
							case _ => Invalid(ValidationError("This is not an ABV value that makes sense"))
						}
					}
				}}
			), 
			/* Why no float or double types?! */
			"ibu" -> number(min=0,max=200),
	  		"ingredients" -> text,
			"grains" -> text,
			"hops" -> text,
			"yeast" -> text,
			"otherings" -> text,
			"styles" -> list(text)
	  )
	  {
  		  (name:String,description:String,abv:String,ibu:Int,ingredients:String,grains:String,hops:String,yeast:String,otherings:String,styles:List[String]) => {
  			  Beer(
				  beerId,
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
			  Some(
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
	  },
	  Map.empty,
	  Nil,
	  None
  )
  {
  }
  
  
	class BreweryForm(breweryId:BeerCrush.BreweryId) extends Form[Brewery](
		  	mapping(
				"name" -> nonEmptyText,
				"address" -> mapping(
					"street" -> text,
					"city" -> text,
					"state" -> text,
					"zip" -> text,
					"country" -> text
				)(Address.apply)(Address.unapply),
				"phone" -> text
				)
			  { (name,address,phone) => Brewery(breweryId,name,address,phone) }
			  { brewery => Some(brewery.name,brewery.address,brewery.phone) },
			  Map.empty,
			  Nil,
			  None
		  )
	{
	}
	  
  def showBeer(breweryId:BeerCrush.BreweryId,beerId:BeerCrush.BeerId) = Action { implicit request => 
	  val beerForm=new BeerForm(breweryId,beerId)
	  val beer=Beer.fromExisting(breweryId + "/" + beerId)
	  val brewery=Brewery.fromExisting(breweryId)
	  
	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		  case AcceptHTMLHeader => Ok(views.html.beer(beer,brewery,beerForm.fill(beer)))
		  case AcceptXMLHeader  => Ok(views.xml.beer(beer,brewery))
		  case AcceptJSONHeader  => Ok(Json.toJson(beer.asJson))
	  }
  }

  def showBrewery(breweryId:BeerCrush.BreweryId) = Action { implicit request =>
	  val breweryForm = new BreweryForm(breweryId)
	  val brewery=Brewery.fromExisting(breweryId)

	  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
		  case AcceptHTMLHeader => Ok(views.html.brewery(brewery,breweryForm.fill(brewery)))
		  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery))
		  case AcceptJSONHeader  => Ok(Json.toJson(brewery.asJson))
	  }
  }
  
  def allBreweries(letter:String="", page: Long) = Action { implicit request =>
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
		  case AcceptJSONHeader  => Ok(Json.toJson(
			  JsObject(List(
				  "meta" -> JsObject(List(
				   "total" -> JsNumber(numFound),
				   "start" -> JsNumber(response.getResults().getStart())
				  )),
				  "breweries" -> JsArray(docs.map(d => JsObject(List(
							"id" -> JsString(d.get("id").toString),
							"name" -> JsString(d.get("name").toString)
				  ))).toList)
		  	  ))
		  ))
  	  }
  }
  
  def allBeers(letter:String="", page: Long) = Action { implicit request =>
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
  
  def search(query:String, page: Long) = Action { implicit request =>
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
  
  def editBeer(breweryId:BeerCrush.BreweryId, beerId:BeerCrush.BeerId) = Action { implicit request => 
	  val beerForm=new BeerForm(breweryId,beerId)
	  beerForm.bindFromRequest.fold(
		  // Handle errors
		  errors => {
			  Ok(views.html.beer(Beer.fromExisting(breweryId + "/" + beerId),Brewery.fromExisting(breweryId),errors))
		  },
	      // Handle successful form submission
	      beer => {
			  // Save the doc
			  beer.save
			  val brewery=Brewery.fromExisting(breweryId)
	  
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.beer(beer,brewery,beerForm.fill(beer)))
				  case AcceptXMLHeader  => Ok(views.xml.beer(beer,brewery))
				  case AcceptJSONHeader  => Ok(Json.toJson(beer.asJson))
			  }
		  }
	  )
  }
  
  def editBrewery(breweryId: BeerCrush.BreweryId) = Action { implicit request =>
	  val breweryForm = new BreweryForm(breweryId)
	  breweryForm.bindFromRequest.fold(
		  errors => {
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.brewery(Brewery.fromExisting(breweryId),errors))
				  case AcceptXMLHeader  => Ok(views.xml.brewery(Brewery.fromExisting(breweryId)))
				  case AcceptJSONHeader  => Ok(Json.toJson(Brewery.fromExisting(breweryId).asJson))
			  }
		  },
		  brewery => {
			  brewery.save
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.brewery(brewery,breweryForm.fill(brewery)))
				  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery))
				  case AcceptJSONHeader  => Ok(Json.toJson(brewery.asJson))
			  }
		  }
	  )
  }
  
  def addBeerPhoto(breweryId: BeerCrush.BreweryId, beerId: BeerCrush.BeerId) = Action { request =>
	  request.body.asMultipartFormData match {
		  case Some(mfd) => { // It's multipartFormData
			  mfd.file("photo").map( uploadedFile => {
				  // val filename=uploadedFile.filename
				  uploadedFile.contentType.map(t =>  t match {
					  case "image/jpg"  => Some(".jpg")
					  case "image/jpeg" => Some(".jpg")
					  case "image/png"  => Some(".png")
					  case _ => None
				  }).map( { case Some(ext) => {
					  // It's a photo!

					  val format=new java.text.SimpleDateFormat("yyyyMMddhhmmssSSSZ")
					  val uniqId=request.session.get("username").getOrElse("Anonymous") + "-" + format.format(new java.util.Date())

					  // Move it to where it belongs
					  uploadedFile.ref.moveTo(new File("/Users/troy/beerdata/photos/"+ breweryId + "/" + beerId + "/" + uniqId + ext),replace=true)
					  // TODO: Make different sizes (thumbnail, small, medium, large, etc.)
					  // TODO: Notify someone so that the photos get backed-up, added to Solr's index, etc.

					  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
						  case AcceptHTMLHeader => Ok("")
						  // case AcceptHTMLHeader => Ok(views.html.beerPhotoUploaded(brewery))
						  case AcceptXMLHeader  => Accepted("")
						  // case AcceptXMLHeader  => Ok(views.xml.beerPhotoUploaded(brewery))
					  }
					  }}
				  ).getOrElse(UnsupportedMediaType("Unsupported image format"))
			}).getOrElse { NotAcceptable("Photo Missing") }
		  }
		  case None => { // TODO: Not multipartFormData, try raw
			  request.body.asRaw match {
				  case Some(raw) => {
					  Ok
				  }
				  case None => { // I can't handle this type for a photo
					  NotAcceptable
				  }
			  }
		  }
	  }
  }

  def showLoginForm = Action { implicit request =>
	  val blankForm=new LoginForm
	  Ok(views.html.login(blankForm))
  }
  
  def login(username: String = "", password: String = "") = Action { implicit request => 
	  val loginForm=new LoginForm
	  loginForm.bindFromRequest.fold(
		  errors => {
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.login(errors))
				  // case AcceptXMLHeader  => Ok(views.xml.login())
			  }
		  },
		  user => {
			  val session=request.session + ("username" -> user.id) + ("name" -> user.name)
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Redirect(routes.Application.index).withSession(session)
				  // case AcceptXMLHeader  => Ok(views.xml.login(loginForm))
			  }
		  }
	)
 }
 
	def logout = Action { implicit request => 
		Redirect(routes.Application.index).withNewSession
	}
	
	def createAccount() = Action { implicit request => 
		val acceptFormat=matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse("")))
		val accountForm=new CreateAccountForm
		accountForm.bindFromRequest.fold(
  		  errors => { // Handle errors
			  acceptFormat match {
				  case AcceptHTMLHeader => Ok(views.html.login(errors))
				  // case AcceptXMLHeader  => Ok(views.xml.login())
			  }
  		  },
  	      user => { // Handle successful form submission
				val session=request.session + ("username" -> user.id) + ("name" -> user.name)
			  
				// Create the account and then display it to the user
				user.save
				
				acceptFormat match {
				  case AcceptHTMLHeader => Redirect(routes.Application.showAccount).withSession(session)
				  // case AcceptXMLHeader  => Ok(views.xml.login(loginForm))
			  }
		  }
		)
	}

	def showUser(userId: String) = Action { implicit request =>
		val acceptFormat=matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse("")))
		User.findUser(userId) match {
			case Some(user) => {
				acceptFormat match {
				  case AcceptHTMLHeader => Ok(views.html.user(user))
				  // case AcceptXMLHeader  => Ok(views.xml.login(loginForm))
			  }
		  }
		  case None => {
			  Ok("")
		  }
		}
	}
	
	def showAccount() = Authenticated {  username =>
		Action { implicit request =>
			val user=User.findUser(username)
			user match {
				case Some(u) => {
					val form=new UserForm(username)
					Ok(views.html.userAccount(username,form.fill(u)))
				}
				case None => Unauthorized
			}
		}
	}
	
	def editAccount() = Authenticated { username =>
		Action { implicit request =>
			val acceptFormat=matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse("")))
			val accountForm=new UserForm(username)
			accountForm.bindFromRequest.fold(
	  		  errors => { // Handle errors
				  acceptFormat match {
					  case AcceptHTMLHeader => Ok(views.html.userAccount(username,errors))
					  // case AcceptXMLHeader  => Ok(views.xml.login())
				  }
	  		  },
	  	      user => { // Handle successful form submission
	  				val session=request.session + ("username" -> user.id) + ("name" -> user.name)
					// Create the account and then display it to the user
					user.save
				
					acceptFormat match {
					  case AcceptHTMLHeader => Ok(views.html.userAccount(username,accountForm.fill(user))).withSession(session)
					  // case AcceptXMLHeader  => Ok(views.xml.login(loginForm))
				  }
			  }
			)
		}
	}
	
}