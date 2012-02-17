package models

class User(
  val username: String,
  val password: String,
  val name: String
) {
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
  
