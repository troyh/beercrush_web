package models

import java.security.MessageDigest
import BeerCrush._

class UserId(id: String) extends Id(id) {
}
object UserId {
implicit def string2id(id: String) = new UserId(id)
}


class User(
  userId: UserId,
  val password: String,
  val name: String,
  val aboutme: String
) extends PersistentObject(userId) {
	lazy val pageURL = "/user/" + id
	def save = {
		val md5pass=this.password match {
			/* Not changing password; get the existing MD5 password, if the user already exists */
			case s if (s.isEmpty) => User.findUser(this.userId).map(_.password).getOrElse("") 
			/* Changing password; MD5 it, never store it in clear text */
			case _ => MessageDigest.getInstance("MD5").digest(this.password.getBytes).map("%02x".format(_)).mkString 
		}
		
		val userXML=
		<user>
			<ctime></ctime>
			<username>{this.id}</username>
			<password enctype="MD5">{md5pass}</password>
			<name>{this.name}</name>
			<aboutme>{this.aboutme}</aboutme>
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
				  (xml \ "name").text,
				  (xml \ "aboutme").text
				))
			}
		else
			None
  }
}
  
