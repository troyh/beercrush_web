package models

import BeerCrush._

class UserId(id: String) extends Id(Some(id)) {
}
object UserId {
implicit def string2id(id: String) = new UserId(id)
}


class User(
  userId: UserId,
  val ctime: java.util.Date,
  val password: String,
  val name: String,
  val aboutme: String
) extends PersistentObject(Some(userId)) {
	lazy val pageURL = "/user/" + id
	def save = {
		val dateFormat=new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat)
		
		val userXML=
		<user>
			<ctime>{dateFormat.format(ctime)}</ctime>
			<username>{id}</username>
			<password>{password}</password>
			<name>{name}</name>
			<aboutme>{aboutme}</aboutme>
		</user>
		
		scala.xml.XML.save("/Users/troy/beerdata/user/" + this.id + ".xml",userXML,"UTF-8",true)
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
				(xml \ "username").text,
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
  
