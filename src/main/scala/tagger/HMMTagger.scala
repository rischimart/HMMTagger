package tagger
import scala.io.Source
import scala.collection.immutable
import scala.math._
import java.io._

/**
 * @author rialmat
 */
class HMMTagger(devfile : String, testfile : String) {
  
  type Bigram[T] = Tuple2[T, T]
  type Trigram[T] = Tuple3[T, T, T]
  type ThreeDArray[T] = Array[Array[Array[T]]]
  private var unigramCounts : Map[String, Int] = Map()
  private var bigramCounts : Map[Bigram[String], Int] = Map()
  private var trigramCounts : Map[Trigram[String], Int] = Map()
  private var emitCounts : Map[Bigram[String], Int] = Map()
  private var words : Set[String] = Set()
  //private var possibleTags: Set[String] = Set()
  private final val TAGS : Array[String] = readStats(devfile)
  private val tests : List[List[String]] = readTests(testfile)
  
  def run {
    writeResults(tests, tests.map(viterbi))
  }
  def updateCounts[A](m : Map[A, Int], key : A, count : Int) : Map[A, Int] = {
    val oldcount = m.getOrElse(key, 0)
    m + (key -> (oldcount + count))
  }
  
  def readStats(filename : String) : Array[String] = {
    var possibleTags: Set[String] = Set()
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
    (List("*", "STOP") ++ possibleTags.toList).toArray
    
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
  
  def getEmissionProb(tagIndex : Int, token : String) : Double = {
    val tok = transformToken(token)
    val tag = TAGS(tagIndex)
    log((emitCounts.getOrElse((tag, tok), 0).toDouble) / unigramCounts(tag))
  }
  
  def getCondintionProb(triIndices : Trigram[Int]) : Double = {
    val bi = (TAGS(triIndices._2), TAGS(triIndices._3))
    val tri = (TAGS(triIndices._1), TAGS(triIndices._2), TAGS(triIndices._3))
    log(bigramCounts(bi).toDouble / trigramCounts(tri))
  }

  def viterbi(test : List[String]) : List[String] = {
    val numTags = TAGS.size
    def getTags(index : Int) : Iterable[Int] = {
      if (index <= 0) Iterable(0)
      else            2 until TAGS.size
    }
    var pi : ThreeDArray[Double] = Array.ofDim(test.size + 1, numTags, numTags)
    var bp : ThreeDArray[Int] = Array.ofDim(test.size + 1)
    pi(0)(0)(0) = 0.0
    for (k <- 1 to test.size) {
      for (u <- getTags(k - 1); v <- getTags(k)) {
        val candidates = for {
          w <- getTags(k - 2)
          p = pi(k - 1)(w)(u) + 
              getCondintionProb((v, w, u)) + 
              getEmissionProb(v, test(k - 1)) 
        } yield (p, w)
        val maxCandidate = candidates.max      
        pi(k)(u)(v) = maxCandidate._1
        bp(k)(u)(v) = maxCandidate._2
      }
    }
    
    val last2Tags = for {
      u <- getTags(test.size - 1)
      v <- getTags(test.size)
    } yield(u, v)
    val (penult, last) = last2Tags.maxBy[Double] {case (u, v) => 
      pi(test.size)(u)(v) + getCondintionProb((1, u, v)) }
    var tags : Array[Int] = Array.ofDim(test.size)
    tags(test.size - 1) = last
    tags(test.size - 2) = penult
    for (n <- test.size - 3 to 0 by -1) {
      tags(n) = bp(n + 3)(tags(n + 1))(tags(n + 2))
    }
    tags.toList.map(TAGS(_))
  }
}