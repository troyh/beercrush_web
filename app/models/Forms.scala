package models

import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.{Constraint, Constraints, Valid, Invalid, ValidationError}
import BeerCrush._

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
  { (username,passwords) => new User(UserId.string2id(username),Some(new java.util.Date()),passwords._1,"","") }
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
  { (password,name,aboutme) => new User(username,Some(new java.util.Date()),password._1,name,aboutme) }
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
			){(street,city,state,zip,country) => Address(street,city,state,zip,country,None,None)}
			{ address => Some(address.street,address.city,address.state,address.zip,address.country) },
			"phone" -> optional(text)
			)
		  { (name,address,phone) => 
			  breweryId match {
				  case None => Brewery(breweryId,name,address,phone) 
				  case Some(id) => Brewery.fromExisting(id) match {
					  case None => Brewery(breweryId,name,address,phone) 
					  case Some(brewery) => {
						  // Merge an existing brewery with this one so we don't lose data not present in the form
						  val newAddress=address.copy(
							  latitude=brewery.address.latitude,
							  longitude=brewery.address.longitude
						  )
						  Brewery(breweryId,name,newAddress,phone)
					  }
				  }
			  }
		  }
		  { brewery => Some(brewery.name,brewery.address,brewery.phone) },
		  Map.empty,
		  Nil,
		  None
	  )
{
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
	
