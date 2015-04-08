package tagger
import scala.io.Source
import scala.collection.immutable
import scala.math._
import java.io._

/**
 * @author rialmat
 */
class HMMTagger(filename : String, testfile : String) {
  
  type Bigram[T] = Tuple2[T, T]
  type Trigram[T] = Tuple3[T, T, T]
  type ThreeDArray = Array[Array[Array[Int]]]
  private var unigramCounts : Map[String, Int] = Map()
  private var bigramCounts : Map[Bigram[String], Int] = Map()
  private var trigramCounts : Map[Trigram[String], Int] = Map()
  private var emitCounts : Map[Bigram[String], Int] = Map()
  private var words : Set[String] = Set()
  private var possibleTags: Set[String] = Set()
  private val tests : List[List[String]] = readTests(testfile)
  
  //private final val TAGS : List[String] = List("O", "I-GENE")
  def updateCounts[A](m : Map[A, Int], key : A, count : Int) : Map[A, Int] = {
    val oldcount = m.getOrElse(key, 0)
    m + (key -> (oldcount + count))
  }
  
  def readStats(filename : String) : Unit = {
    for (line <- Source.fromFile(filename).getLines()) {
      val tokens = line.split(" ")

      val count= tokens(0).toInt
      if (tokens(1) == "WORDTAG") {
        emitCounts = updateCounts(emitCounts, (tokens(2), tokens(3)), count)
        possibleTags = possibleTags + (tokens(2), tokens(3))
        
        words += tokens(3)
      } else if (tokens(1) == "1-GRAM") {
        unigramCounts = updateCounts(unigramCounts, tokens(2), count)
      } else if (tokens(2) == "2-GRAM") {
        bigramCounts = updateCounts(bigramCounts, (tokens(2), tokens(3)), count)
      } else {
        trigramCounts = updateCounts(trigramCounts, (tokens(2), tokens(3), tokens(4)), count)
      }
    }
  }
  
  def readTests(testfile : String) : List[List[String]] = {
    def mkTests(itr : Iterator[String]) : List[List[String]] = {
      val (head, rest) = itr.span (!_.isEmpty())
      val test = head.toList
      if (test.isEmpty) List()
      else test :: mkTests(rest)
    }
    mkTests(Source.fromFile(testfile).getLines())
  }
  
  def writeResults(tests : List[List[String]], assignments: List[List[String]]) : Unit = {
    def mkResult(tests : List[List[String]], assignments: List[List[String]]) : List[String] = {
      for {
        test <- tests
        tags <- assignments
        val z = for {
          token <- test
          tag <- tags
        } yield (token ++ " " ++ tag)
      } yield z.mkString("\n")
    }
    val resultFile = new PrintWriter(new File(testfile ++ ".out"))
    resultFile.write(mkResult(tests, assignments).mkString("\n\n"))
    resultFile.close()
  }
  
  
  def containsDigit(token : String) : Boolean = token.exists (_.isLetter)
  def isAllCaps(token : String) : Boolean = token.forall (_.isUpper)
  def isLastCap(token : String) : Boolean = !token.isEmpty() && token.last.isUpper
  def isRare(token : String) : Boolean = words.contains(token)
  
  def transformToken(token : String) : String = {
    if (!isRare(token))  token
    else if (isAllCaps(token))  "_ALLCAP_"
    else if (isLastCap(token))  "_LASTCAP_"
    else if (containsDigit(token))  "_NUM_"
    else  "_RARE_"
  }
  
  def getEmissionProb(tag : String, token : String) : Double = {
    val tok = transformToken(token)
    log((emitCounts.getOrElse((tag, tok), 0).toDouble) / unigramCounts(tag))
  }
  
  def getCondintionProb(tri : Trigram[String], bi : Bigram[String]) : Double = {
    log(bigramCounts(bi).toDouble / trigramCounts(tri))
  }

  def updatePi(pi : ThreeDArray, index : Trigram[Int]) {
    
  }
  def viterbi(test : List[String]) = {
    val numTags = possibleTags.size
    var pi : ThreeDArray = Array.ofDim(test.size + 1, numTags + 1, numTags + 1)
    
  }
  
}