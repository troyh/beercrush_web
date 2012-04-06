package models

import BeerCrush._
import play.api.libs.json._
import scala.xml._

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
  id: User.Id,
  val ctime: Option[java.util.Date],
  val password: String,
  val name: String,
  val aboutme: String
) extends XmlFormat with JsonFormat {

	lazy val pageURL = "/user/" + id
	// def id=userId
	// lazy val descriptiveNameForId = name
	// def dupe(id:Id,ctime:java.util.Date) = this.copy(userId=new UserId(id),ctime=Some(ctime))
	
	def toJSON: JsObject = JsObject(
		(
			"name" -> JsString(name) ::
			"aboutme" -> JsString(aboutme) ::
			Nil
		)
	)
	
	def toXML = transform(<user/>)

	import SuperNode._
	def transform(nodes: NodeSeq): NodeSeq = for (n <- nodes) yield n match {
		case u @ <user>{_*}</user> => 
			u.asInstanceOf[Elem] % 
				Attribute("","id",id.toString,Null) % 
				Attribute("","ctime",new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(ctime.getOrElse(new java.util.Date)),Null) copy(
			child=(for (k <- u.withMissingChildElements(Seq("name","password","aboutme")).child) yield k match {
				case <name>{_*}</name> => <name>{name}</name>
				case <password>{_*}</password> => <password>{password}</password>
				case <aboutme>{_*}</aboutme> => <aboutme>{aboutme}</aboutme>
				case <ctime>{_*}</ctime> => <ctime/>
				case <username>{_*}</username> => <username/>
				case other => other
			}))
		case other => other
	}
}
  
object User {

	case class Id(s: String) extends UniqueId(s) {
		def fileLocation = BeerCrush.datadir + "/user/" + id + ".xml"
	}

	object Id {
		implicit def string2id(id: String): User.Id = new User.Id(id)
		object Undefined extends User.Id("")
	}

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
				User.Id((xml \ xmlTagUsername).headOption.map{s => s.text}.getOrElse(username)),
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
  
