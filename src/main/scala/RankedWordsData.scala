import java.io.{FileInputStream, PrintWriter}
import java.util.zip.GZIPInputStream
import scala.util.{Failure, Success, Try, Using}
import scalaz.Scalaz._

/**
 * Recupera la info de las palabras que son posibles de utilizar en Wordle, junto con un coeficiente que representa la
 * frecuencia de uso de la palabra en Google Books. En caso de no contar con los datos procesados, prepara los datos.
 * el comando para recuperar la información es `loadSolverData`
 */
object RankedWordsData {

    def rankingExtractor(s:String): Option[(String,Int)] =
        s.split('\t').toList match {
            case word ::(_ :+ lastRecord) if word.length == 5 && word.forall(_.isLetter) => Some(word.toLowerCase -> lastRecord.split(',').apply(1).toInt)
            case _ => None
        }

    def importNGramDatabase(): Try[Map[String, Int]] = {
        // Cargamos los contadores de las palabras para utilizarlo como aproximación de la pertinencia de la palabra como respuesta
        val nGramFilePaths: Seq[String] = {
            val nGramFileCount: Int = 24
            val dataPath: String = "data/ngrams-db"
            (0 until nGramFileCount).map(i => f"$dataPath/1-$i%05d-of-$nGramFileCount%05d.gz")
        }

        Using.Manager { use =>
            nGramFilePaths map { path =>
                val is = use(new GZIPInputStream(new FileInputStream(path)))
                scala.io.Source.fromInputStream(is)
                  .getLines()
                  .flatMap(rankingExtractor)
                  .toMap
            } reduce( _ |+| _ )
        }
    }

    // Definimos las palabras posibles
    def importWordleWords(): Try[Set[String]] = {
        val wordleWordsFilePath = "data/wordle_word_list.txt"
        Using(scala.io.Source.fromFile(wordleWordsFilePath)) {
            _
              .getLines()
              .filter( word => word.length == 5 && word.forall(_.isLetter) )
              .map(_.toLowerCase)
              .toSet
        }
    }

    def tryGetRankedWordleChoices(): Try[Map[String, Int]] = {
        val nGramWordRanking: Try[Map[String,Int]] = importNGramDatabase()
        val wordleWords: Try[Set[String]] = importWordleWords()

        (wordleWords, nGramWordRanking) match {
            case (Success(word),Success(rankings)) =>
                Try( word
                  .map( w => w -> rankings.getOrElse(w, 0) )
                  .toMap)
            case (Failure(w), Failure(r)) => Failure(new Exception (s"La carga de los datos ha fracasado. Razón: $w \n $r"))
            case (_,Failure(e)) => Failure(new Exception (s"La carga de los datos ha fracasado. Razón: $e "))
            case (Failure(e), _) => Failure(new Exception (s"La carga de los datos ha fracasado. Razón: $e "))

        }
    }

    def cacheWordleWords(m: Map[String,Int]): Try[Unit] = {
        lazy val contenido: Seq[String] = m.toSeq.map(i => s"${i._1}\t${i._2}\n")
        Using(new PrintWriter("data/wordle_cached.txt")) { pw =>
            contenido.foreach(line => pw.write(line))
        }
    }

    def loadWordleCache(): Try[Map[String,Int]] = Using(scala.io.Source.fromFile("data/wordle_cached.txt")) {
        _
          .getLines()
          .map{ _.split('\t') match { case Array(k,v) => k -> v.toInt }}
          .toMap

    }

    def loadSolverData(): Try[Map[String, Int]] = {
        println("Fetching Wordle data...")
        loadWordleCache() match {
            case Success(m) =>
                Try(m)
            case Failure(e) =>
                println(s"La carga del fichero ha fallado($e), generando el archivo de nuevo...")
                tryGetRankedWordleChoices() match {
                    case Success(m) =>
                        cacheWordleWords(m)
                        Try(m)
                    case Failure(e) => Failure(e)
                }
        }
    }

}
