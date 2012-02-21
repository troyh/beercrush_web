package models

import BeerCrush._

class UserId(id: String) extends Id(id) {
}
object UserId {
implicit def string2id(id: String) = new UserId(id)
}


class User(
  userId: UserId,
  val password: String,
  val name: String
) extends PersistentObject(userId) {
	lazy val pageURL = "/user/" + id
	def save = {
		val userXML=
		<user>
			<ctime></ctime>
			<username>{this.id}</username>
			<password>{this.password}</password>
			<name>{this.name}</name>
		</user>
		scala.xml.XML.save("/Users/troy/beerdata/user/" + this.id + ".xml",userXML,"UTF-8",true)
	}
}
  
object User {
  def findUser(username:String): Option[User] = {
		  val filename=new java.io.File("/Users/troy/beerdata/user/" + username + ".xml")
		  if (filename.exists()) {
			val xml=scala.xml.XML.loadFile(filename)
			  Some(new User(
				  (xml \ "username").text,
				  (xml \ "password").text,
				  (xml \ "name").text
				))
			}
		else
			None
  }
}
  
