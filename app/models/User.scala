package models

import BeerCrush._
import play.api.libs.json._

class UserId(id: String) extends Id(Some(id)) {
}
object UserId {
	implicit def string2id(id: String): UserId = new UserId(id)
}


class User(
  userId: UserId,
  val ctime: java.util.Date,
  val password: String,
  val name: String,
  val aboutme: String
) extends PersistentObject(Some(userId)) with XmlFormat with JsonFormat {
	lazy val pageURL = "/user/" + id
	def save = scala.xml.XML.save("/Users/troy/beerdata/user/" + id.get + ".xml",asXML,"UTF-8",true)
	
	def asJson: JsObject = JsObject(
		(
			"name" -> JsString(name) ::
			"aboutme" -> JsString(aboutme) ::
			Nil
		)
	)
	
	def asXML: xml.Node = {
		<user>
			<username>{id.getOrElse("")}</username>
			<ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(ctime)}</ctime>
			<name>{name}</name>
			<password>{password}</password>
			<aboutme>{aboutme}</aboutme>
		</user>
	}
}
  
object User {
	def findUser(username:String): Option[User] = {
		val filename=new java.io.File("/Users/troy/beerdata/user/" + username + ".xml")
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
				new UserId((xml \ "username").headOption.map{s => s.text}.getOrElse(username)),
				ctime,
				(xml \ "password").text,
				(xml \ "name").text,
				(xml \ "aboutme").text
			))
		}
		else
			None
  }
}
  
