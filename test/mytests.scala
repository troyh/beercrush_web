import play.api.test._
import play.api.test.Helpers._
import org.specs2.mutable._


class MySpec extends Specification {

	def xmlRequestTest(url: String, test: String) = {
		val headers=FakeHeaders(Map(
			ACCEPT -> List("text/xml")
		))
		val Some(result)=routeAndCall(FakeRequest(GET, url, headers,null))

		status(result) must equalTo(OK)
		contentType(result) must beSome("text/xml")
		charset(result) must beSome("utf-8")
		contentAsString(result) must contain(test)
	}

	"Home page" in {
	  val result = controllers.Application.index(FakeRequest())
  
	  status(result) must equalTo(OK)
	  contentType(result) must beSome("text/html")
	  charset(result) must beSome("utf-8")
	  contentAsString(result) must contain("Beer Crush")
	}
	
	"Beer XML page" in { 
		for (pair <- List(
			("/Alaskan-Brewing-Co/Barleywine","Barleywine"),
			("/Alaskan-Brewing-Co/Big-Nugget","Big Nugget"),
			("/Alesmith-Brewing-Co/Decadence-Anniversary-Ale", "Decadence Anniversary")
			)
		) yield xmlRequestTest(pair._1,pair._2)
	}
	"Brewery XML page" in {
		for (pair <- List(
			("/Avery-Brewing-Company", "Avery Brewing Company"),
			("/Alaskan-Brewing-Co", "Alaskan Brewing"),
			("/Queen-City-Brewing-Ltd","Queen City Brewing")
			)
		) yield xmlRequestTest(pair._1,pair._2)
	}
	
}
