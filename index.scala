//> using platform scala-js
//> using dep org.scala-js::scalajs-dom::2.8.0
//> using dep com.raquo::laminar::17.0.0
//> using dep com.github.j-mie6::parsley::5.0.0-M6
//> using scala 3.5.0-RC1

import org.scalajs.dom.*
import com.raquo.laminar.api.L.*
import parsley.Parsley, Parsley.*, parsley.character.*
import scala.scalajs.js.Date
import parsley.Success
import parsley.Failure

val app =
  val text = Var(
    Option(window.localStorage.getItem("planet-of-the-apes")).getOrElse("")
  )

  val currentYear = new Date().getFullYear().toInt

  val yearVar = Var(currentYear)

  div(
    cls := "container mx-auto p-4 bg-white grid grid-cols-1",
    p("Title"),
    input(
      placeholder := "start typing...",
      onInput.mapToValue --> text,
      value <-- text,
      cls := "m-4 p-4 text-xl w-8/12 border-2 border-slate-700 border-solid"
    ),
    p("Year by which the thing was definitely released"),
    input(
      placeholder := "year",
      maxLength := 4,
      value <-- yearVar.signal.map(_.toString),
      onInput.mapToValue
        .map(_.toInt)
        .filter(y => y >= 1971 && y <= currentYear) --> yearVar,
      cls := "m-4 p-4 text-xl w-8/12 border-2 border-slate-700 border-solid"
    ),
    child <-- text.signal
      .combineWith(yearVar.signal)
      .map:
        case (text, year) =>
          grammar(year).parse(text.trim.toLowerCase()) match
            case Success(x) =>
              div(
                cls := "bg-green-700 p-4 text-white text-xl font-bold",
                x match
                  case Outcome.OriginalMovie =>
                    "Congratulations! It is indeed one of the original movies"
                  case Outcome.MovieRemake =>
                    "Congratulations! It is indeed the movie remake"
                  case Outcome.MovieReboot =>
                    "Congratulations! It is indeed one of the reboots"
                  case Outcome.AnimatedSeries =>
                    "Congratulations! It is indeed the original animated series"
              )
            case Failure(f) =>
              div(
                cls := "bg-red-700 p-4 text-white text-xl font-bold",
                s"Unfortunately this is not something from the Planet of the Apes media franchise that was released before year $year :("
              )
    ,
    text.signal --> { res =>
      window.localStorage.setItem("planet-of-the-apes", res)
    }
  )
end app

enum Outcome:
  case OriginalMovie, MovieRemake, MovieReboot, AnimatedSeries

def grammar(year: Int) =
  inline def token(str: String) =
    atomic(string(str.toLowerCase())) <~ whitespaces.void

  val PLANET_OF_THE_APES = token("Planet of the Apes")
  var FOR = token("for")
  var FROM = token("from")
  var THE = token("the")
  var OF = token("of")
  val SPACE = token(" ")
  val BENEATH = token("Beneath")
  val ESCAPE = token("Escape")
  val CONQUEST = token("Conquest")
  val BATTLE = token("Battle")
  val RISE = token("Rise")
  val DAWN = token("Dawn")
  val WAR = token("War")
  val KINGDOM = token("Kingdom")
  val RETURN = token("Return")
  val TO = token("to")

  extension (p: Parsley[Outcome])
    def releasedIn(releaseYear: Int) =
      Option.when(year >= releaseYear)(p)

  extension (p: Parsley[String])
    def originalMovie = p.as(Outcome.OriginalMovie)
    def remake = p.as(Outcome.MovieRemake)
    def reboot = p.as(Outcome.MovieReboot)
    def animated = p.as(Outcome.AnimatedSeries)

  val originalSeries =
    List(
      PLANET_OF_THE_APES.originalMovie.releasedIn(1968),
      (BENEATH ~> THE ~> PLANET_OF_THE_APES).originalMovie.releasedIn(1970),
      (ESCAPE ~> FROM ~> THE ~> PLANET_OF_THE_APES).originalMovie
        .releasedIn(1971),
      (CONQUEST ~> OF ~> THE ~> PLANET_OF_THE_APES).originalMovie
        .releasedIn(1972),
      (BATTLE ~> FOR ~> THE ~> PLANET_OF_THE_APES).originalMovie
        .releasedIn(1973)
    ).flatten

  val remakes =
    List(
      PLANET_OF_THE_APES.remake.releasedIn(2001)
    ).flatten

  val reboots =
    List(
      (RISE ~> OF ~> THE ~> PLANET_OF_THE_APES).reboot.releasedIn(2011),
      (DAWN ~> OF ~> THE ~> PLANET_OF_THE_APES).reboot
        .releasedIn(2014),
      (WAR ~> FOR ~> THE ~> PLANET_OF_THE_APES).reboot
        .releasedIn(2017),
      (KINGDOM ~> OF ~> THE ~> PLANET_OF_THE_APES).reboot.releasedIn(2024)
    ).flatten

  val animatedSeries =
    List(
      (RETURN ~> TO ~> THE ~> PLANET_OF_THE_APES).animated.releasedIn(1975)
    ).flatten

  (originalSeries ++ remakes ++ reboots ++ animatedSeries)
    .foldLeft[Parsley[Outcome]](empty)((res, next) => res <|> next)

end grammar

@main def hello =
  renderOnDomContentLoaded(document.getElementById("content"), app)

end hello
