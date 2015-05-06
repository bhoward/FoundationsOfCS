package chapter11

import scala.util.parsing.combinator.RegexParsers

// Loosely based on Figure 11.27 of Aho & Ullman
object CombinatorParser {
  sealed trait Balance
  case object Empty extends Balance
  case class Parens(first: Balance, second: Balance) extends Balance

  object BalancedParens extends RegexParsers {
    override def skipWhitespace = false

    def B: Parser[Balance] =
      ( "(" ~ B ~ ")" ~ B ^^ { case _ ~ first ~ _ ~ second => Parens(first, second) }
      | ""                ^^ { case _ => Empty }
      )

    def apply(input: String): Option[(Balance, Char)] = parse(B, input) match {
      case Success(result, leftover) =>
        // Extract next leftover character, or '$'
        val next = if (leftover.atEnd) '$' else leftover.first
        Some((result, next))
      case _: NoSuccess => None
    }
  }
}