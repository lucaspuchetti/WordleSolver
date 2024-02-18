
case class Letter(character: Char, pos: Int)
abstract class Word {
    def word: String
    def enumerated: Seq[Letter] = word.toSeq.zipWithIndex.map( (c, p) => Letter(c, p) )
}

case class Guess(word: String) extends Word
case class Answer(word: String) extends Word

enum MatchLevel:
  case PartialMatch, NoMatch, ExactMatch

case class Decision(letter: Letter, matchLevel: MatchLevel)

object WordleLogic {
    def getAnswer: Answer = {
        println("RESPUESTA:")
        Answer(io.StdIn.readLine().toLowerCase)
    }

    def getGuess: Guess = {
        println("ADIVINACIÓN:")
        Guess(io.StdIn.readLine().toLowerCase)
    }

    private def resolveForFullMatch(l: Letter, opts: Seq[Letter]): (Decision, Seq[Letter]) =
        opts filter (_ == l) match
            case Nil => (Decision(l, MatchLevel.NoMatch), opts)
            case x   => (Decision(l, MatchLevel.ExactMatch), opts diff x )

    private def resolveForPartialMatch(l: Letter, opts: Seq[Letter]): (Decision, Seq[Letter]) =
        opts filter (_.character == l.character) match
            case Nil => (Decision(l, MatchLevel.NoMatch), opts)
            case pms => (Decision(l, MatchLevel.PartialMatch), opts diff pms.take(1) )

    private def getDecisionsFromFunction(sourceLetterSeq: Seq[Letter], targetLetterSeq: Seq[Letter], resolveFunc: (Letter, Seq[Letter]) => (Decision, Seq[Letter])): (Seq[Decision], Seq[Letter]) =
        sourceLetterSeq.foldLeft(Seq[Decision](), targetLetterSeq)(
            (acc: (Seq[Decision], Seq[Letter]), letter: Letter) =>
                val (decisions: Seq[Decision], current_options: Seq[Letter]) = acc
                val (current_decision: Decision, new_options: Seq[Letter]) = resolveFunc(letter, current_options)
                (decisions :+ current_decision, new_options)
        )

    def resolveWord(guess: Guess, answer: Answer): Seq[Decision] =
        val (decisionFromFullMatch, remainingOptions) = getDecisionsFromFunction(
            guess.enumerated,
            answer.enumerated,
            resolveForFullMatch
        )
        val lettersForPartialMatch = decisionFromFullMatch
          .filter( _.matchLevel == MatchLevel.NoMatch )
          .map( _.letter )
        val (decisionFromPartialMatch, _) = getDecisionsFromFunction(
            lettersForPartialMatch,
            remainingOptions,
            resolveForPartialMatch
        )

        val finalDecisions = decisionFromFullMatch.filter( _.matchLevel == MatchLevel.ExactMatch) ++  decisionFromPartialMatch

        finalDecisions.sortBy( _.letter.pos )

}