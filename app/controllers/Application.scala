package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Constraints, Valid, Invalid, ValidationError}
import play.api.libs.json._
import play.api.mvc._
import play.api.mvc.Security._
import org.apache.solr._
import scalaj.collection.Imports._
import models._
import java.io._
import BeerCrush._	
import HTTPHelpers._

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

object Application extends Controller {


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
	  { user => Some(user.id.get,user.password)}.verifying( user => {
			  val existingUser=User.findUser(user.id.get)
			  existingUser.isDefined && user.password==existingUser.get.password
	  }),
	  Map.empty,
	  Nil,
	  None
  )

  class NewUserForm extends Form[User](
  	  mapping(
  		  "username" -> nonEmptyText,
  		  "passwords" -> tuple(
  			  "password1" -> text(minLength=6),
  			  "password2" -> text
  		  ).verifying("Passwords don't match", passwords => passwords._1 == passwords._2)
  	  )
  	  { (username,passwords) => new User(UserId.string2id(username),new java.util.Date(),passwords._1,"","") }
  	  { user => Some(user.id.get,(user.password,user.password))}.verifying(
  		  "This username is already taken",
  		  user => !User.findUser(user.id.get).isDefined
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
		  "name" -> text,
		  "aboutme" -> text
	  )
	  { (password,name,aboutme) => new User(username,new java.util.Date(),password._1,name,aboutme) }
	  { user => Some(("",""),user.name,user.aboutme)},
	  Map.empty,
	  Nil,
	  None
  )
 
  class BeerForm(beerId: Option[BeerId]) extends Form[Beer](
	  mapping(
			"name" -> nonEmptyText,
			"description" -> optional(text),
			"abv" -> optional(text.verifying(
					Constraints.pattern("^\\s*\\d+(\\.[\\d]+)?\\s*%?\\s*$".r,"Percentage of alcohol by volume","Invalid ABV value")
				).transform[Double](
					/* Strip off any % sign, it's implied, and then convert to Double */
					{ s => s.replace("%","").toDouble }, 
					{ i => i.toString }
				).verifying(
					FormConstraints.min(0),
					FormConstraints.max(25)
			)),
			"ibu" -> optional(number(min=0,max=200)),
	  		"ingredients" -> optional(text),
			"grains" -> optional(text),
			"hops" -> optional(text),
			"yeast" -> optional(text),
			"otherings" -> optional(text),
			"styles" -> optional(list(text))
	  )
	  {
  		  (name:String,description:Option[String],abv:Option[Double],ibu:Option[Int],ingredients:Option[String],grains:Option[String],hops:Option[String],yeast:Option[String],otherings:Option[String],styles:Option[List[String]]) => {
  			  Beer(
				  beerId = beerId,
				  name = name,
				  description = description,
				  abv = abv,
				  ibu = ibu,
				  ingredients = ingredients,
				  grains = grains,
				  hops = hops,
				  yeast = yeast,
				  otherings = otherings,
				  styles = styles.map{_.map(s => new BeerStyle(s,s))}
			  )
  		  }
	  }
	  {
		  beer => {
			  Some(
				  beer.name,
				  beer.description,
				  beer.abv,
				  beer.ibu,
				  beer.ingredients,
				  beer.grains,
				  beer.hops,
				  beer.yeast,
				  beer.otherings,
				  beer.styles.map(_.map(s => s.id))
			  )
		  }
	  },
	  Map.empty,
	  Nil,
	  None
  )
  {
  }
  
  
	class BreweryForm(breweryId:Option[BreweryId]) extends Form[Brewery](
		  	mapping(
				"name" -> nonEmptyText,
				"address" -> mapping(
					"street" -> optional(text),
					"city" -> optional(text),
					"state" -> optional(text),
					"zip" -> optional(text),
					"country" -> optional(text)
				)(Address.apply)(Address.unapply),
				"phone" -> optional(text)
				)
			  { (name,address,phone) => Brewery(breweryId,name,address,phone) }
			  { brewery => Some(brewery.name,brewery.address,brewery.phone) },
			  Map.empty,
			  Nil,
			  None
		  )
	{
	}
	  
  def showBeer(beerId:BeerId) = Action { implicit request => 
	  Beer.fromExisting(beerId) match {
		  case None => NotFound
		  case Some(beer) => {
			  val beerForm=new BeerForm(beer.beerId)
	  
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.beer(Some(beer),beerForm.fill(beer)))
				  case AcceptXMLHeader  => Ok(views.xml.beer(beer))
				  case AcceptJSONHeader  => Ok(Json.toJson(beer.asJson))
			  }
		  }
	  }
  }

  def showBrewery(breweryId:Option[BreweryId]) = Action { implicit request =>
	  breweryId match {
		  case None => NotFound
		  case Some(id) => Brewery.fromExisting(id) match {
			  case None => NotFound
			  case Some(brewery: Brewery) => {
				  val breweryForm = new BreweryForm(breweryId)
			  
				  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
					  case AcceptHTMLHeader => Ok(views.html.brewery(brewery,breweryForm.fill(brewery)))
					  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery))
					  case AcceptJSONHeader  => Ok(Json.toJson(brewery.asJson))
				  }
			  }
		  }
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
  
  def newBeer(breweryId:BreweryId) = editBeer(None)
  
  def editBeer(beerId:Option[BeerId]) = Action { implicit request => 
	  val beerForm=new BeerForm(beerId)
	  beerForm.bindFromRequest.fold(
		  // Handle errors
		  errors => {
			  Ok(views.html.beer(beerId.map{Beer.fromExisting(_)}.getOrElse(None),errors))
		  },
	      // Handle successful form submission
	      beer => {
			  // Save the doc
			  beer.save
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.beer(Some(beer),beerForm.fill(beer)))
				  case AcceptXMLHeader  => Ok(views.xml.beer(beer))
				  case AcceptJSONHeader  => Ok(Json.toJson(beer.asJson))
			  }
		  }
	  )
  }
  
  def newBrewery = editBrewery(None)

  def editBrewery(breweryId: Option[BreweryId]) = Action { implicit request =>
	  val f=new BreweryForm(breweryId)
	  f.bindFromRequest.fold(
		  errors => {
			  val brewery: Option[Brewery] = breweryId.map{Brewery.fromExisting(_)}.getOrElse(None)
			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.brewery(brewery.get,errors))
				  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery.get))
				  case AcceptJSONHeader  => Ok(Json.toJson(brewery.get.asJson))
			  }
		  },
		  brewery => {
			  brewery.save

			  matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse(""))) match {
				  case AcceptHTMLHeader => Ok(views.html.brewery(brewery,f.fill(brewery)))
				  case AcceptXMLHeader  => Ok(views.xml.brewery(brewery))
				  case AcceptJSONHeader  => Ok(Json.toJson(brewery.asJson))
			  }
		  }
	  )
	  
  }
  
  def addBeerPhoto(beerId: BeerId) = Action { request =>
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
					  uploadedFile.ref.moveTo(new File("/Users/troy/beerdata/photos/"+ beerId + "/" + uniqId + ext),replace=true)
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
			  val session=request.session + ("username" -> user.id.get) + ("name" -> user.name)
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
	
	def createAccount = Action { implicit request => 
		val acceptFormat=matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse("")))
		val newUserForm=new NewUserForm()
		newUserForm.bindFromRequest.fold(
			errors => { // Handle errors
				acceptFormat match {
					case AcceptHTMLHeader => Ok(views.html.login(errors))
					// case AcceptXMLHeader  => Ok(views.xml.login())
				}
			},
			newUser => { // Handle successful form submission
				val session=request.session + ("username" -> newUser.id.get) + ("name" -> newUser.name)
			  
				// Create the account and then display it to the user
				newUser.save
				
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
	  				val session=request.session + ("username" -> user.id.get) + ("name" -> user.name)

					// Make a new User object as a merging of the submitted form's User and an existing User (if any)
					val userToSave=User.findUser(username) match {
						case Some(existingUser) => {
							new User(
								username,
								existingUser.ctime,
								user.password match {
									/* Not changing password; use the existing MD5 password */
									case s if (s.isEmpty) => existingUser.password
									/* Changing password; MD5 it, never store it in clear text */
									case s => java.security.MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
								},
								user.name,
								user.aboutme
							)
						}
						case None => new User(
							username,
							new java.util.Date(),
							java.security.MessageDigest.getInstance("MD5").digest(user.password.getBytes).map("%02x".format(_)).mkString,
							user.name,
							user.aboutme
						)
					}
					
					userToSave.save
				
					acceptFormat match {
					  case AcceptHTMLHeader => Ok(views.html.userAccount(username,accountForm.fill(userToSave))).withSession(session)
					  // case AcceptXMLHeader  => Ok(views.xml.login(loginForm))
				  }
			  }
			)
		}
	}
	
}