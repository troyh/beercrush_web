package models

import BeerCrush._
import play.api.libs.json._

class UserId(id: String) extends Id(Some(id)) {
}
object UserId {
	implicit def string2id(id: String): UserId = new UserId(id)
}


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
			{ctime.map { d => <ctime>{new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(d)}</ctime>}}
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
				Some(ctime),
				(xml \ "password").text,
				(xml \ "name").text,
				(xml \ "aboutme").text
			))
		}
		else
			None
  }
}
  
