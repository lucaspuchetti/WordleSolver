import scala.util.{Failure, Success}

object WordleSolver extends App {
    // Cargamos los datos:
    val wordleWords: Map[String, Int] = RankedWordsData.loadSolverData() match {
        case Failure(e) => throw e
        case Success(m) => m
    }

    println(s"The file contains ${wordleWords.size} words\nHere's a sneak peek:\n${wordleWords.take(5).map("\t" + _ + "\n").mkString}")
}
