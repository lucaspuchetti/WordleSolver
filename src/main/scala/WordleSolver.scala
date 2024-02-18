import MatchLevel.ExactMatch
import WordleLogic.{getGuess, getAnswer, resolveWord}

object WordleSolver extends App {
    // Cargamos los datos:
    val wordleWords = RankedWordsData.wordleWords.toSeq.sortWith(_._2 > _._2)

    println(s"The file contains ${wordleWords.size} words\nHere's a sneak peek:\n${wordleWords.take(5).map { case k -> v => f"\t$k%s :$v%016.15f%n"}.mkString}")
    /*
    lazy val contenido: Seq[String] = wordleWords.map(i => s"${i._1},${i._2}\n")
    Using(new PrintWriter("data/wordle_adjusted.csv")) { pw =>
      pw.write("word,weight\n")
        contenido.foreach(line => pw.write(line))
    }
     */
    val answer = getAnswer
    var continue = true

    for (_ <- 1 to 5) {
        if (continue) {
            val v = resolveWord(getGuess, answer)
            println(v.mkString(" "))
            continue = !v.forall( _.matchLevel == ExactMatch )
        } else print("")

    }

}
