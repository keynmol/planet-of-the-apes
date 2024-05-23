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
  val yearVar = Var(new Date().getFullYear().toInt)

  div(
    input(
      placeholder := "start typing...",
      onInput.mapToValue --> text,
      value <-- text
    ),
    input(
      placeholder := "year",
      maxLength := 4,
      value <-- yearVar.signal.map(_.toString),
      onInput.mapToValue
        .map(_.toInt)
        .filter(y => y >= 1971 && y <= 2024) --> yearVar
    ),
    child <-- text.signal
      .combineWith(yearVar.signal)
      .map:
        case (text, year) =>
          grammar(year).parse(text) match
            case Success(x) => div(x.toString())
            case Failure(f) =>
              div(
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

  val originalSeries =
    List(
      Option.when(year >= 1968)(PLANET_OF_THE_APES.as(Outcome.OriginalMovie)),
      Option.when(year >= 1970)(
        (BENEATH ~> THE ~> PLANET_OF_THE_APES).as(Outcome.OriginalMovie)
      ),
      Option.when(year >= 1971)(
        (ESCAPE ~> FROM ~> THE ~> PLANET_OF_THE_APES).as(Outcome.OriginalMovie)
      ),
      Option.when(year >= 1972)(
        (CONQUEST ~> OF ~> THE ~> PLANET_OF_THE_APES).as(Outcome.OriginalMovie)
      ),
      Option.when(year >= 1972)(
        (BATTLE ~> FOR ~> THE ~> PLANET_OF_THE_APES).as(Outcome.OriginalMovie)
      )
    ).flatten

  val remake =
    List(
      Option.when(year >= 2001)(PLANET_OF_THE_APES.as(Outcome.MovieRemake))
    ).flatten

  (originalSeries ++ remake).foldLeft[Parsley[Outcome]](empty)((res, next) =>
    res <|> next
  )

end grammar

@main def hello =
  renderOnDomContentLoaded(document.getElementById("content"), app)

end hello
