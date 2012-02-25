package HTTPHelpers

import util.parsing.combinator._

// This parsing code from http://www.suryasuravarapu.com/2011/04/scala-parser-combinators-win.html
case class AcceptHeader(mediaType: String, mediaSubType: String, qualityFactor: Float)

object AcceptHeaderParser extends JavaTokenParsers {
  lazy val accept: Parser[List[AcceptHeader]] = rep1sep(acceptEntry, ",")
  lazy val acceptEntry: Parser[AcceptHeader] = (mediaType <~ "/") ~ mediaSubType ~ opt(qualityFactor) ^^ {
    case t ~ st ~ Some(q) => AcceptHeader(t, st, q.toFloat)
    case t ~ st ~ None => AcceptHeader(t, st, 1.0F)
  }
  lazy val wordRegex = """[\w+\-*]*""".r
  lazy val mediaType = wordRegex
  lazy val mediaSubType = wordRegex
  lazy val qualityFactor = ";" ~> "q" ~> "=" ~> floatingPointNumber

  def parse(input: String): List[AcceptHeader] = parseAll(accept, input).getOrElse(Nil)
}

