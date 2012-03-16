package models

import BeerCrush._
import play.api.libs.json._
import scala.xml.{NodeSeq, Node}

class UserId(id: String) extends Id(Some(id)) {
}
object UserId {
	implicit def string2id(id: String): UserId = new UserId(id)
}

/**
  * Representation of registered users
  *
  * @param userId id for the user, e.g., "troyh"
  * @param ctime Creation time
  * @param password User's password
  * @param name Friendly display name for the user
  * @param aboutme Descriptive text about the user
  */
case class User(
  userId: UserId,
  val ctime: Option[java.util.Date],
  val password: String,
  val name: String,
  val aboutme: String
) extends XmlFormat with JsonFormat with Storage.Saveable {
	lazy val pageURL = "/user/" + id
	def id=Some(userId)
	lazy val descriptiveNameForId = name
	def dupe(id:Id,ctime:java.util.Date) = this.copy(userId=new UserId(id),ctime=Some(ctime))
	
	def toJSON: JsObject = JsObject(
		(
			"name" -> JsString(name) ::
			"aboutme" -> JsString(aboutme) ::
			Nil
		)
	)
	
	def toXML = transform(<user/>)

	def transform(nodes: NodeSeq): NodeSeq = applyValuesToXML(
		nodes,
		Map(
			(User.xmlTagUser, { orig => <user id={userId}>{applyValuesToXML(orig.child,Map(
				( User.xmlTagUsername, { orig => <username>{userId}</username> })
				,(User.xmlTagCtime,    { orig => if (ctime.isDefined) <ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(ctime.get)}</ctime> else orig })
				,(User.xmlTagName,     { orig => <name>{name}</name> })
				,(User.xmlTagPassword, { orig => <password>{password}</password> })
				,(User.xmlTagAboutme,  { orig => <aboutme>{aboutme}</aboutme> })
			))}</user>})
		)
	)
}
  
object User {
	
	private final val xmlTagUser 	 = "user"
	private final val xmlTagUsername = "username"
	private final val xmlTagCtime 	 = "ctime"
	private final val xmlTagName 	 = "name"
	private final val xmlTagPassword = "password"
	private final val xmlTagAboutme  = "aboutme"
	
	/**
	  * Finds an existing user
	  *
	  * @param username The user's username (id)
	  */
	def findUser(username:String): Option[User] = {
		val filename=new java.io.File("/Users/troy/beerdata/user/" + username + ".xml") // TODO: Use Storage class
		if (filename.exists()) {
			val xml=scala.xml.XML.loadFile(filename)
			val dateFormat=new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat)
			val ctime=try { 
				dateFormat.parse((xml \ "ctime").text)
			}
			catch {
				case _ => new java.util.Date()
			}
			  
			Some(new User(
				new UserId((xml \ xmlTagUsername).headOption.map{s => s.text}.getOrElse(username)),
				Some(ctime),
				(xml \ xmlTagPassword).text,
				(xml \ xmlTagName    ).text,
				(xml \ xmlTagAboutme ).text
			))
		}
		else
			None
  }
}
  
