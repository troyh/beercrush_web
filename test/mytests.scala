import play.api.test._
import play.api.test.Helpers._
import org.specs2.mutable._


class MySpec extends Specification {

	"Home page" in {
	  val result = controllers.Application.index(FakeRequest())
  
	  status(result) must equalTo(OK)
	  contentType(result) must beSome("text/html")
	  charset(result) must beSome("utf-8")
	  contentAsString(result) must contain("Beer Crush")
	}
	
	"Beer XML page" in {
		val headers=FakeHeaders(Map(
			ACCEPT -> List("text/xml")
		))
		val Some(result)=routeAndCall(FakeRequest(GET, "/Alaskan-Brewing-Co/Barleywine", headers,null))

		println(headers.data.toString)
		status(result) must equalTo(OK)
		contentType(result) must beSome("text/xml")
		charset(result) must beSome("utf-8")
		contentAsString(result) must contain("Barleywine")
	}
	
}
