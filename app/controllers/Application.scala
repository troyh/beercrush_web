package controllers

import play.api._
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

	def indexDoc(doc: Any, id: UniqueId[_]) = {
		val fields: Map[String,Any] = doc match {
			case review: BeerReview => Map(
				"doctype" -> "beerreview",
				"id"      -> id,
				"rating"  -> review.rating,
				"user_id" -> ReviewId.userIdFromReviewId(id.asInstanceOf[ReviewId]),
				"beer_id" -> ReviewId.beerIdFromReviewId(id.asInstanceOf[ReviewId]),
				"ctime"   -> review.ctime.get
			)
			case brewery: Brewery => Map(
				"doctype" -> "brewery",
				"id"      -> id,
				"name"    -> brewery.name
			)
			case beer: Beer => Map(
				"doctype" -> "beer",
				"id"      -> id,
				"name"    -> beer.name,
				"brewery" -> (try { beer.brewery.get.id } catch { case _ => "" })
			)
			case _ => Map()
		}
						
		// TODO: convert Dates UTC because Solr wants all dates as UTC
		val xml=
		<add><doc>{ fields.map { pair => 
			<field name={pair._1}>{ pair._2 match {
				case v: String => v
				case v: java.util.Date => new java.text.SimpleDateFormat(BeerCrush.SolrDateFormat).format(v)
				case None => ""
				case Some(v) => v.toString
				case v  => v.toString
			}}</field>
		}}</doc></add>

		val s=new org.apache.solr.client.solrj.impl.StreamingUpdateSolrServer("http://localhost:8983/solr",1,1)
		val response=s.request(new org.apache.solr.client.solrj.request.DirectXmlRequest("/update",xml.toString))
		val commit= <commit waitFlush="false" waitSearcher="false"/>
		val response2=s.request(new org.apache.solr.client.solrj.request.DirectXmlRequest("/update",commit.toString))
	}

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


	def responseFormat(implicit request: play.api.mvc.Request[_]) = matchAcceptHeader(AcceptHeaderParser.parse(request.headers(ACCEPT)))

	def index = Action { implicit request => 
		Ok(views.html.index("Beer Crush"))
	}

	def showBeer(beerId:Beer.Id) = Action { implicit request => 
		Beer(beerId) match {
			case None => NotFound
			case Some(beer) => {
				responseFormat match {
					case HTML => Ok(views.html.beer(Some(beer),new BeerForm fill(beer),new BeerReviewForm(None)))
					case XML  => Ok(beer.toXML)
					case JSON => Ok(Json.toJson(beer.toJSON))
				}
			}
		}
	}

	def showBrewery(breweryId:Brewery.Id) = Action { implicit request =>
		Brewery(breweryId) match {
			case None => NotFound
			case Some(brewery) => {
				responseFormat match {
					case HTML => Ok(views.html.brewery(brewery,new BreweryForm().fill(brewery)))
					case XML  => Ok(brewery.toXML)
					case JSON  => Ok(Json.toJson(brewery.toJSON))
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

  def newBeer(breweryId:Brewery.Id) = editBeer(Left(breweryId))

  def editBeer(beerOrBreweryId:Either[Brewery.Id,Beer.Id]) = Action { implicit request => 
	  val beerForm=new BeerForm
	  beerForm.bindFromRequest.fold(
		  // Handle errors
		  errors => {
			  Ok(views.html.beer(beerOrBreweryId.fold(_ => None,Beer(_)),errors,new BeerReviewForm(None)))
		  },
	      beer => { // Handle successful form submission
			  // It could either be a new beer or editing an existing beer. A BeerId or BreweryId
			  // determines this.
			  val beerId=beerOrBreweryId match {
				  case Left(breweryId) => Beer.Id.newUniqueId(breweryId, beer.name)
				  case Right(beerId) => beerId
			  }

			  // Save the doc
			  BeerCrush.save(beer.copy(id=beerId))

			  future { indexDoc(beer, beer.id) } // Index in Solr

			  responseFormat match {
				  case HTML => Ok(views.html.beer(Some(beer),beerForm.fill(beer),new BeerReviewForm(None)))
				  case XML  => Ok(views.xml.beer(beer))
				  case JSON  => Ok(Json.toJson(beer.toJSON))
			  }
		  }
	  )
  }
  
  def newBrewery = editBrewery(None)

  def editBrewery(breweryId: Option[Brewery.Id]) = Action { implicit request =>
	  val f=new BreweryForm
	  f.bindFromRequest.fold(
		  errors => {
			  val brewery: Option[Brewery] = breweryId.map{Brewery(_)}.getOrElse(None)
			  responseFormat match {
				  case HTML => Ok(views.html.brewery(brewery.get,errors))
				  case XML  => Ok(views.xml.brewery(brewery.get))
				  case JSON  => Ok(Json.toJson(brewery.get.toJSON))
			  }
		  },
		  brewery => {
			  BeerCrush.save(brewery)

			  future { indexDoc(brewery, brewery.id) } // Index in Solr

			  responseFormat match {
				  case HTML => Ok(views.html.brewery(brewery,f.fill(brewery)))
				  case XML  => Ok(views.xml.brewery(brewery))
				  case JSON  => Ok(Json.toJson(brewery.toJSON))
			  }
		  }
	  )
	  
  }
  
  def addBeerPhoto(beerId: Beer.Id) = Action { implicit request =>
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

					  responseFormat match {
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
  
	def newBeerReview(beerId: Beer.Id) = editBeerReview(None)
	
	def editBeerReview(reviewId: Option[ReviewId]) = Authenticated { username =>
		Action { implicit request => 

			val form=new BeerReviewForm(reviewId)
			form.bindFromRequest.fold(
				errors => {
					responseFormat match {
						case HTML => Ok(views.html.beerReview(reviewId.map{BeerReview.fromExisting(_)}.getOrElse(None),errors))
						case XML => BadRequest
						case JSON => BadRequest
					}
				},
				review => {
					BeerCrush.save(review)

					future { indexDoc(review, review.id) } // Asynchronously index the review in Solr

					responseFormat match {
						case HTML => Redirect(routes.Application.showBeerReview(review.id.toString))
						case XML => Ok(review.toXML)
						case JSON => Ok(Json.toJson(review.toJSON))
					}
				}
			)
		}
	}
	
	def showBeerReview(reviewId: ReviewId) = Action { implicit request =>
		val review=BeerReview.fromExisting(reviewId)
		responseFormat match {
			case HTML => Ok(views.html.beerReview(review,new BeerReviewForm(None)))
			case XML => Ok(review.get.toXML)
			case JSON => Ok(Json.toJson(review.get.toJSON))
		}
	}
	
	def showBeerReviews(beerId: Beer.Id, page: Long) = Action { implicit request =>
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
				docs.map{ r => 
					val p=r.get("id").asInstanceOf[String].split("/")
					new BeerReview(
						Some(ReviewId( Tuple2(Beer.Id(p(0),p(1)), User.Id(p(4)))))
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
				,Beer(beerId)
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
			  val session=request.session + ("username" -> user.id.toString) + ("name" -> user.name)
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
				val session=request.session + ("username" -> newUser.id.toString) + ("name" -> newUser.name)
				  
				// Create the account and then display it to the user
				BeerCrush.save(newUser)

				future { indexDoc(newUser, newUser.id) } // Index in Solr

				responseFormat match {
				  case HTML => Redirect(routes.Application.showUser(newUser.id.toString)).withSession(session)
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
				  case HTML => Ok(views.html.user(user,new UserForm(User.Id(userId)).fill(user)))
				  case XML  => Ok(user.toXML.map { _ match { 
					  case e @ scala.xml.Elem(prefix,"user",attribs,scope,children @ _*) => <user>{ children.map { _ match {
  						  case <password>{_*}</password> => scala.xml.Text("") // Remove the password!
  						  case other => other
					  }}}</user> % attribs
					  case other => other
				  }})
				  case JSON => Ok(Json.toJson(user.toJSON))
				}
			}
			case None => NotFound
		}
	}
	
	def editUser(user: String) = Authenticated { username =>
		Authorized(username == user) { // The user must be this user, users can only edit their own info
			Action { implicit request =>

				val accountForm=new UserForm(User.Id(username))
				accountForm.bindFromRequest.fold(
		  		  errors => { // Handle errors
					  responseFormat match {
						  case HTML => Ok(views.html.userAccount(User.Id(username),errors))
						  case XML  => BadRequest
						  case JSON  => BadRequest
					  }
		  		  },
		  	      user => { // Handle successful form submission
		  				val session=request.session + ("username" -> user.id.toString) + ("name" -> user.name)

						// Make a new User object as a merging of the submitted form's User and an existing User (if any)
						val userToSave=User.findUser(username) match {
							case Some(existingUser) => {
								new User(
									User.Id(username),
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
								User.Id(username),
								Some(new java.util.Date()),
								java.security.MessageDigest.getInstance("MD5").digest(user.password.getBytes).map("%02x".format(_)).mkString,
								user.name,
								user.aboutme
							)
						}
					
						BeerCrush.save(userToSave)

						// TODO: Index in Solr
						future { indexDoc(userToSave, userToSave.id) } // Index in Solr
				
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
	
	def showAllStyles = Action { implicit request =>

		def outputSubstylesXML(style: BeerStyle): scala.xml.NodeSeq = {
			style.substyles.flatMap({
				case Some(id) => BeerStyle.fromExisting(new BeerStyle.Id(id.toString)) match {
					case Some(style: BeerStyle) => style.transform(<style>{outputSubstylesXML(style)}</style>)
					case None => scala.xml.Text("")
				}
				case None => scala.xml.Text("")
			})
		}
		
		BeerStyle.fromExisting(BeerStyle.RootId) match {
			case None => NotFound
			case Some(rootStyle) => responseFormat match {
				case HTML => Ok(views.html.beerstyles(rootStyle))
				case _  => Ok(rootStyle.transform(<style>{outputSubstylesXML(rootStyle)}</style>))
				// case JSON => TODO
			}
		}
		
	}
	
	def showStyle(styleId: BeerStyle.Id) = Action { implicit request => 
		BeerStyle.fromExisting(styleId) match {
			case Some(style: BeerStyle) => responseFormat match {
				case HTML => Ok(views.html.beerstyle(style))
				// case XML =>
				// case JSON =>
			}
			case None => NotFound
		}
	}
	
}