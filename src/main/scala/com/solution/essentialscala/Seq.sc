import scala.util.Sorting

/*
-- Synonym of length is Seq is called
    *   size
-- Two methods to get first element of a CustomList
    *   head
    *

-- What method can be used to display the elements of the sequence as a string?
    *   toSeq

-- What methodof Option canbe used to determine whether the option contains a value
    *   isDefined
 */

//Create a Seq containing the Strings "cat", "dog", and "penguin". Bind it to the name animal
val animals: Seq[String] = Seq("cat", "dog", "animal")
// Append the element "tyrannosaurus" to animals and prepend the element "mouse".
val modAnimals = "mouse" +: animals :+ "tyrannosaurus"

//movie database
case class Film(
                 name: String,
                 yearOfRelease: Int,
                 imdbRating: Double)
case class Director(
                     firstName: String,
                     lastName: String,
                     yearOfBirth: Int,
                     films: Seq[Film])

val memento = new Film("Memento", 2000, 8.5)
val darkKnight = new Film("Dark Knight", 2008, 9.0)
val inception = new Film("Inception", 2010, 8.8)

val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
val unforgiven = new Film("Unforgiven", 1992, 8.3)
val granTorino = new Film("Gran Torino", 2008, 8.2)
val invictus = new Film("Invictus", 2009, 7.4)
val predator = new Film("Predator", 1987, 7.9)
val dieHard = new Film("Die Hard", 1988, 8.3)
val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

val eastwood = new Director("Clint", "Eastwood", 1930, Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino,
  invictus))
val mcTiernan = new Director("John", "McTiernan", 1951, Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))
val nolan = new Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))
val someGuy = new Director("Just", "Some Guy", 1990,
  Seq())
val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

// AcceptaparameternumberOfFilmsoftypeInt—findalldirectorswho have directed more than numberOfFilms:
def directorFilter(numberOfFilms: Int): Seq[Director] = {
  directors.filter(dir => dir.films.length > numberOfFilms)
}

// Accept a parameter year of type Int—find a director who was born before that year
def bornBefore(year: Int): Seq[Director] = directors.filter(dir => dir.yearOfBirth < year)

def filterBothYearAndFilms(numberOfFilms: Int, year: Int) = directorFilter(numberOfFilms) intersect bornBefore(year)

filterBothYearAndFilms(3, 1970)

def sortInOrder(ascending: Boolean = true): Seq[Director] = {
  if(ascending) directors.sortWith((a, b) => a.yearOfBirth > b.yearOfBirth)
  else directors.sortWith((a, b) => a.yearOfBirth < b.yearOfBirth)
}