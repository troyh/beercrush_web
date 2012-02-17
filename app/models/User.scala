package models

class User(
  val username: String,
  val password: String,
  val name: String
) {
}
  
object User {
  def findUser(username:String): Option[User] = {
	  if (username!="troy")
	  None
	  else
	  Some(new User(username,"hello","1st Person"))
  }
}
  
