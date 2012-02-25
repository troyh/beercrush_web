package models

class BeerStyle(val id: String, val name: String) {
	lazy val pageURL="/style/" + id
}

