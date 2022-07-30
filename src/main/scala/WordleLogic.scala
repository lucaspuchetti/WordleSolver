
import scala.annotation.tailrec

abstract class Word {
    def word: String
    def enumerated: Seq[(Char, Int)] = word.toSeq.zipWithIndex
}

case class Guess(word: String) extends Word
case class Answer(word: String) extends Word

abstract class MatchLevel
object NoMatch extends MatchLevel {
    override def toString: String = "No Match!"
}
object PartialMatch extends MatchLevel {
    override def toString: String = "Partial Match"
}
object ExactMatch extends MatchLevel {
    override def toString: String = "Exact Match!"
}

object WordleLogic {
    def getAnswer: Answer = {
        println("RESPUESTA:")
        Answer(io.StdIn.readLine().toLowerCase)
    }

    def getGuess: Guess = {
        println("ADIVINACIÓN:")
        Guess(io.StdIn.readLine().toLowerCase)
    }

    // Función que devuelve el resultado de la adivinación.
    @tailrec
    def resolveMatchedCharacters(resolveType: MatchLevel, resolveGuessedChar: ((Char,Int), Seq[(Char,Int)]) => (Boolean,Int))(a: Seq[(Char, Int)], g: Seq[(Char,Int)])(r: Seq[(Char, Int, MatchLevel)] = Nil): Seq[(Char, Int, MatchLevel)] = {
        a match {
            case h +: t =>
                val (resolvePredicate, resolvedPosition) = resolveGuessedChar(h, g)
                if (resolvePredicate) {
                    // Removemos los caracteres adivinados
                    val currentGuess = g.filter(_ != (h._1, resolvedPosition))
                    resolveMatchedCharacters(resolveType, resolveGuessedChar)(t, currentGuess)((h._1, resolvedPosition, resolveType) +: r)
                } else {
                    resolveMatchedCharacters(resolveType, resolveGuessedChar)(t, g)(r)
                }
            case Nil => r
        }
    }

    // Definimos las reglas para encontrar aciertos
    def findExactMatch(letter: (Char,Int), possibilities: Seq[(Char,Int)]): (Boolean,Int) = {
        if (possibilities.contains(letter)) (true, letter._2) else (false, -1)
    }

    def findPartialMatch(letter: (Char,Int), possibilities: Seq[(Char,Int)]): (Boolean,Int) = {
        val partialMatches = possibilities.filter( _._1 == letter._1)
        if (partialMatches.nonEmpty) (true, partialMatches.head._2) else (false,-1)
    }

    // Primero  resolvemos los aciertos, luego los parciales y marcamos el resto como no adivinados.
    def getWordleMatch(g: Guess, a: Answer) : Seq[MatchLevel] = {

        val exactMatches = resolveMatchedCharacters(resolveType = ExactMatch, resolveGuessedChar = findExactMatch)(a.enumerated, g.enumerated)()
        // Removemos las letras adivinadas antes de buscar parciales.
        val unguessedAndUnanswered: Word => Seq[(Char,Int)] = y => y.enumerated.filterNot {
            exactMatches.map(x => (x._1, x._2)).contains(_)
        }
        val unguessed = unguessedAndUnanswered(g)
        val unanswered = unguessedAndUnanswered(a)

        val partialMatches = resolveMatchedCharacters(resolveType = PartialMatch, resolveGuessedChar = findPartialMatch)(unanswered,unguessed)()

        // Recuperamos el resto, ordenamos y terminamos

        val notMatched = g.enumerated.filterNot { l =>
            (exactMatches ++ partialMatches).map(x => (x._1,x._2)).contains(l)
        } map {
            l => (l._1,l._2, NoMatch)
        }

        (exactMatches ++ partialMatches ++ notMatched).sortWith(_._2 < _._2).map(_._3)

    }

    }
