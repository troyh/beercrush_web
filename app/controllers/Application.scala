package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc._
import play.api.mvc.Security._
import org.apache.solr._
import scalaj.collection.Imports._
import models._
import java.io._
import BeerCrush._	
import HTTPHelpers._
import scala.actors.Futures._

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

	case class AuthorizedRequest[A](
	  val request: Request[A]
	) extends WrappedRequest(request)

	def Authorized[A](p: BodyParser[A])(test: => Boolean)(f: AuthorizedRequest[A] => Result) = {
		Action(p) { request =>
			test match {
				case true => f(AuthorizedRequest(request))
				case false => Unauthorized
			}
		}
	}

	// Overloaded method to use the default body parser
	def Authorized(test: => Boolean)(f: AuthorizedRequest[AnyContent] => Result): Action[AnyContent]  = {
	  Authorized(parse.anyContent)(test)(f)
	}
	

	val solr=new org.apache.solr.client.solrj.impl.CommonsHttpSolrServer("http://localhost:8983/solr")

	def index = Action { implicit request => 
		Ok(views.html.index("Beer Crush"))
	}

	sealed abstract class AcceptHeaderType 
	case object XML extends AcceptHeaderType
	case object JSON extends AcceptHeaderType
	case object HTML extends AcceptHeaderType

  def matchAcceptHeader(ahl: List[AcceptHeader]): AcceptHeaderType = {
	  ahl match {
		  case AcceptHeader("text","html",_) :: rest => HTML
		  case AcceptHeader("text","xml",_) :: rest => XML
		  case AcceptHeader("application","json",_) :: rest => JSON
		  case head :: rest => matchAcceptHeader(rest)
		  case Nil => HTML
	  }
  }


	def responseFormat(implicit request: play.api.mvc.Request[_]) = matchAcceptHeader(AcceptHeaderParser.parse(request.headers.get("accept").getOrElse("")))

  def showBeer(beerId:BeerId) = Action { implicit request => 
	  Beer.fromExisting(beerId) match {
		  case None => NotFound
		  case Some(beer) => {
			  val beerForm=new BeerForm(beer.beerId)
	  
			  responseFormat match {
				  case HTML => Ok(views.html.beer(Some(beer),beerForm.fill(beer),new BeerReviewForm(None)))
				  case XML  => Ok(views.xml.beer(beer))
				  case JSON  => Ok(Json.toJson(beer.asJson))
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
			  
				  responseFormat match {
					  case HTML => Ok(views.html.brewery(brewery,breweryForm.fill(brewery)))
					  case XML  => Ok(views.xml.brewery(brewery))
					  case JSON  => Ok(Json.toJson(brewery.asJson))
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
  	  responseFormat match {
  		  case HTML => Ok(views.html.allBreweries(
			docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>),
	  	  	numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
	  		page))
  		  case XML  => Ok(views.xml.allBreweries(
  			docs.map(d => <brewery><id>{d.get("id")}</id><name>{d.get("name")}</name></brewery>),
  	  	  	numFound,
  	  		response.getResults().getStart()))
		  case JSON  => Ok(Json.toJson(
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
	  responseFormat match {
		case HTML => Ok(views.html.allBeers(
			docs.map(d => <beer><id>{d.get("id")}</id><name>{d.get("name")}</name></beer>),
			numFound / MAX_ROWS + (if (numFound % MAX_ROWS == 0) 0 else 1),
			page))
		case XML  => Ok(views.xml.allBeers(
			docs.map(d => <beer><id>{d.get("id")}</id><name>{d.get("name")}</name></beer>),
			numFound,
			response.getResults().getStart()))
		case JSON => Ok(Json.toJson(JsObject(
			"totalBeers" -> JsNumber(numFound) ::
			"start" -> JsNumber(response.getResults().getStart()) ::
			"beers" -> JsArray(
				docs.map(b => JsObject(
					"id" -> JsString(b.get("id").toString) ::
					"name" -> JsString(b.get("name").toString) ::
					Nil
				)).toList
			) ::
			Nil
			)))
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
  	  responseFormat match {
  		case HTML => Ok(views.html.search(
			query,
			(numFound + (MAX_ROWS-1)) / MAX_ROWS,
			page,
			docs))
  		case XML  => Ok(views.xml.search(
			query,
			numFound,
			response.getResults().getStart(),
			docs))
		case JSON => Ok(Json.toJson(JsObject(
			"query" -> JsString(query) ::
			"totalResults" -> JsNumber(numFound) ::
			"start" -> JsNumber(response.getResults().getStart()) ::
			"results" -> JsArray(
				docs.map(b => JsObject(
					"id" -> JsString(b.get("id").toString) ::
					"name" -> JsString(b.get("name").toString) ::
					Nil
				)).toList
			) ::
			Nil
		)))
  	  }
  }
  
  def newBeer(breweryId:BreweryId) = editBeer(None)
  
  def editBeer(beerId:Option[BeerId]) = Action { implicit request => 
	  val beerForm=new BeerForm(beerId)
	  beerForm.bindFromRequest.fold(
		  // Handle errors
		  errors => {
			  Ok(views.html.beer(beerId.map{Beer.fromExisting(_)}.getOrElse(None),errors,new BeerReviewForm(None)))
		  },
	      // Handle successful form submission
	      beer => {
			  // Save the doc
			  beer.save
			  responseFormat match {
				  case HTML => Ok(views.html.beer(Some(beer),beerForm.fill(beer),new BeerReviewForm(None)))
				  case XML  => Ok(views.xml.beer(beer))
				  case JSON  => Ok(Json.toJson(beer.asJson))
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
			  responseFormat match {
				  case HTML => Ok(views.html.brewery(brewery.get,errors))
				  case XML  => Ok(views.xml.brewery(brewery.get))
				  case JSON  => Ok(Json.toJson(brewery.get.asJson))
			  }
		  },
		  brewery => {
			  brewery.save

			  responseFormat match {
				  case HTML => Ok(views.html.brewery(brewery,f.fill(brewery)))
				  case XML  => Ok(views.xml.brewery(brewery))
				  case JSON  => Ok(Json.toJson(brewery.asJson))
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
				  }).map( { ext => 
					  // It's a photo!

					  val format=new java.text.SimpleDateFormat("yyyyMMddhhmmssSSSZ")
					  val uniqId=request.session.get("username").getOrElse("Anonymous") + "-" + format.format(new java.util.Date())

					  // Move it to where it belongs
					  uploadedFile.ref.moveTo(new File("/Users/troy/beerdata/photos/"+ beerId + "/" + uniqId + ext),replace=true)
					  // TODO: Make different sizes (thumbnail, small, medium, large, etc.)
					  // TODO: Notify someone so that the photos get backed-up, added to Solr's index, etc.

					  responseFormat(request) match {
						  case HTML => Ok("")
						  // case HTML => Ok(views.html.beerPhotoUploaded(brewery))
						  case XML  => Accepted("")
						  // case XML  => Ok(views.xml.beerPhotoUploaded(brewery))
						  case JSON  => Accepted("")
					  }
				  }).getOrElse(UnsupportedMediaType("Unsupported image format"))
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
  
	class BeerReviewForm(val reviewId: Option[ReviewId]) extends Form[BeerReview](
		mapping(
			"rating" -> number(min=1,max=5),
			"balance" -> optional(number(min=1,max=10)),
			"aftertaste" -> optional(number(min=1,max=10)),
			"flavors" -> optional(list(text)),
			"drank_when" -> optional(date),
			"drank_where" -> optional(text),
			"wouldDrinkAgain" -> optional(boolean),
			"text" -> optional(text)
		)
		{ (rating,balance,aftertaste,flavors,drank_when,drank_where,wouldDrinkAgain,text) => BeerReview(
			None
			,None
			,rating
			,balance
			,aftertaste
			,flavors
			,drank_when
			,drank_where
			,wouldDrinkAgain
			,text
		)}
		{ review => Some(review.rating,review.balance,review.aftertaste,review.flavors,review.when,review.where,review.wouldDrinkAgain,review.text) },
		Map.empty,
		Nil,
		None
	)
	
	def newBeerReview(beerId: BeerId) = editBeerReview(ReviewId.fromBeerId(beerId))
	
	def editBeerReview(reviewId: ReviewId) = Authenticated { username =>
		Action { implicit request => 

			val form=new BeerReviewForm(Some(reviewId))
			form.bindFromRequest.fold(
				errors => {
					responseFormat match {
						case HTML => Ok(views.html.beerReview(BeerReview.fromExisting(reviewId),errors))
						case XML => BadRequest
						case JSON => BadRequest
					}
				},
				review => {
					val saveId=if (reviewId.isComplete) reviewId else reviewId.setUser(username)
					val reviewToSave=review.copy(
						id=Some(saveId),
						ctime={if (review.ctime.isEmpty) Some(new java.util.Date()) else review.ctime}
					)

					Storage.save(reviewToSave,Some(saveId))

					// Asynchronously index the review in Solr
					future {
						val s=new org.apache.solr.client.solrj.impl.StreamingUpdateSolrServer("http://localhost:8983/solr",1,1)
						
						// TODO: convert ctime in review to UTC because Solr wants all dates as UTC
						val xml=
						<add>
							<doc>
								<field name="doctype">beerreview</field>
								<field name="id">{reviewToSave.id.get.toString}</field>
								<field name="rating">{reviewToSave.rating.toString}</field>
								<field name="user_id">{ReviewId.userIdFromReviewId(reviewToSave.id.get).toString}</field>
								<field name="beer_id">{ReviewId.beerIdFromReviewId(reviewToSave.id.get).toString}</field>
								<field name="ctime">{new java.text.SimpleDateFormat(BeerCrush.SolrDateFormat).format(reviewToSave.ctime.get)}</field>
							</doc>
						</add>
						val response=s.request(new org.apache.solr.client.solrj.request.DirectXmlRequest("/update",xml.toString))
						
						val commit= <commit waitFlush="false" waitSearcher="false"/>
						val response2=s.request(new org.apache.solr.client.solrj.request.DirectXmlRequest("/update",commit.toString))
					}

					responseFormat match {
						case HTML => Redirect(routes.Application.showBeerReview(reviewToSave.id.get))
						case XML => Ok(reviewToSave.asXML)
						case JSON => Ok(Json.toJson(reviewToSave.asJson))
					}
				}
			)
		}
	}
	
	def showBeerReview(reviewId: ReviewId) = Action { implicit request =>
		val review=BeerReview.fromExisting(reviewId)
		responseFormat match {
			case HTML => Ok(views.html.beerReview(review,new BeerReviewForm(None)))
			case XML => Ok(review.get.asXML)
			case JSON => Ok(Json.toJson(review.get.asJson))
		}
	}
	
	def showBeerReviews(beerId: BeerId, page: Long) = Action { implicit request =>
		val MAX_ROWS=20
		val parameters=new org.apache.solr.client.solrj.SolrQuery()
		parameters.set("q","doctype:beerreview AND beer_id:" + beerId.toString);
		// parameters.set("defType","dismax")
		// parameters.set("qf","name")
		parameters.setStart(((page-1) * MAX_ROWS).toInt)
		parameters.setRows(MAX_ROWS)
		val response=solr.query(parameters)
		val numFound=response.getResults().getNumFound()
		val docs=response.getResults().asScala
		
		responseFormat match {
			case HTML => Ok(views.html.beerReviews(
				docs.map{ r => new BeerReview(
					Some(ReviewId(r.get("id").asInstanceOf[String]))
					,Some(r.get("ctime").asInstanceOf[java.util.Date])
					,r.get("rating").asInstanceOf[Int]
					,None
					,None
					,None
					,None
					,None
					,None
					,None
				) }
				,Beer.fromExisting(beerId)
				,numFound
				,(numFound + (MAX_ROWS-1)) / MAX_ROWS
				,page
			))
			case XML => Ok
			case JSON => Ok
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
			  responseFormat match {
				  case HTML => Ok(views.html.login(errors))
				  case XML  => Unauthorized
				  case JSON  => Unauthorized
			  }
		  },
		  user => {
			  val session=request.session + ("username" -> user.id.get) + ("name" -> user.name)
			  responseFormat match {
				  case HTML => Redirect(routes.Application.index).withSession(session)
				  case XML  => Ok.withSession(session)
				  case JSON  => Ok.withSession(session)
			  }
		  }
	)
 }
 
	def logout = Action { implicit request => 
		Redirect(routes.Application.index).withNewSession
	}

	def newUser = Action { implicit request => 
		val newUserForm=new NewUserForm()
		newUserForm.bindFromRequest.fold(
			errors => { // Handle errors
				responseFormat match {
					case HTML => Ok(views.html.login(errors))
					case XML  => BadRequest
					case JSON  => BadRequest
				}
			},
			newUser => { // Handle successful form submission
				val session=request.session + ("username" -> newUser.id.get) + ("name" -> newUser.name)
				  
				// Create the account and then display it to the user
				newUser.save
					
				responseFormat match {
				  case HTML => Redirect(routes.Application.showUser(newUser.id.get)).withSession(session)
				  case XML  => Ok.withSession(session)
				  case JSON  => Ok.withSession(session)
			  }
			}
		)
	}

	def showUser(userId: String) = Action { implicit request =>
		User.findUser(userId) match {
			case Some(user) => {
				responseFormat match {
				  case HTML => Ok(views.html.user(user,new UserForm(userId).fill(user)))
				  case XML  => Ok(user.asXML match {
					  case <user>{ e @ _* }</user> => <user>{e.filterNot(_.label.equals("password"))}</user>
				  })
				  case JSON => Ok(Json.toJson(user.asJson))
				}
			}
			case None => NotFound
		}
	}
	
	def editUser(user: String) = Authenticated { username =>
		Authorized(username == user) { // The user must be this user, users can only edit their own info
			Action { implicit request =>

				val accountForm=new UserForm(username)
				accountForm.bindFromRequest.fold(
		  		  errors => { // Handle errors
					  responseFormat match {
						  case HTML => Ok(views.html.userAccount(username,errors))
						  case XML  => BadRequest
						  case JSON  => BadRequest
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
				
						responseFormat match {
						  case HTML => Ok(views.html.user(userToSave,accountForm.fill(userToSave))).withSession(session)
						  case XML  => Ok.withSession(session)
						  case JSON  => Ok.withSession(session)
					  }
				  }
				)
			}
		}
	}
	
}