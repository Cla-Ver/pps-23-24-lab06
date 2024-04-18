package ex2
import Pair.*
import scala.collection.Map

enum Question:
    case Relevance
    case Significance
    case Confidence
    case Final

trait Review:
    def article: Int
    def relevance: Int
    def significance: Int
    def confidence: Int
    def finalScore: Int

object Review:
    def apply(article: Int, relevance: Int, significance: Int, confidence: Int, finalScore: Int): Review = ReviewImpl(article, relevance, significance, confidence, finalScore)

private case class ReviewImpl(override val article: Int, override val relevance: Int, override val significance: Int, override val confidence: Int, override val finalScore: Int) extends Review

trait ConferenceReviewing():
  def loadReview(review: Review): ConferenceReviewing
  def averageFinalScore(article: Int): Double
  def orderedScores(article: Int, question: Question): List[Int]
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScore(): Map[Int, Double]
  def numberOfReviews(): Int


object ConferenceReviewing:
    def apply(reviews: List[Review]): ConferenceReviewing = ConferenceReviewingImpl()

private class ConferenceReviewingImpl(reviews: List[Pair[Int, Map[Question, Int]]] = Nil) extends ConferenceReviewing():

    private var this.reviews: List[Pair[Int, Map[Question, Int]]] = reviews

    override def numberOfReviews(): Int = this.reviews.length

    override def loadReview(review: Review): ConferenceReviewing = ConferenceReviewingImpl(this.reviews concat List(Pair(review.article, Map(Question.Relevance -> review.relevance, Question.Significance -> review. significance, Question.Confidence -> review.confidence, Question.Final -> review.finalScore))))

    override def orderedScores(article: Int, question: Question): List[Int] = this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(question).get).sorted

    override def averageWeightedFinalScore(): Map[Int, Double] = this.reviews.map(p => p.getX()).distinct.map(p => (p, singleArticleWeightedFinalScore(p))).toMap

    override def acceptedArticles(): Set[Int] = this.reviews.map(p => p.getX()).distinct.filter(p => accepted(p)).toSet
    override def sortedAcceptedArticles(): List[Pair[Int, Double]] = acceptedArticles().map(p => Pair(p, averageFinalScore(p))).toList.sorted((e1, e2) => e1.getY().compareTo(e2.getY()))

    override def averageFinalScore(article: Int): Double = mean(this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Final).orElse(Some(0)).get))
    
    private def mean(list: List[Int]): Double = list.sum.toDouble / list.length
    private def accepted(article: Int): Boolean = averageFinalScore(article) >= 5.0 && this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Relevance)).max.orElse(Some(0)).get >= 8.0
    private def singleArticleWeightedFinalScore(article: Int): Double = mean(this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Relevance).get * p.getY().get(Question.Final).get / 10))

@main def main() = 
    var conference = ConferenceReviewing(Nil)
    conference = conference.loadReview(Review(1, 9, 9, 6, 9)) // 5.4 
    conference = conference.loadReview(Review(2, 9, 9, 10, 9)) // 9.0
    conference = conference.loadReview(Review(2, 4, 6, 10, 6)) // 6.0
    conference = conference.loadReview(Review(3, 3, 3, 3, 3)) // 0.9
    conference = conference.loadReview(Review(3, 4, 4, 4, 4)) // 1.6
    conference = conference.loadReview(Review(4, 6, 6, 6, 6)) // 3.6
    conference = conference.loadReview(Review(4, 7, 7, 8, 7)) // 5.6

    //println(conference.numberOfReviews())
    println(conference.orderedScores(4, Question.Relevance))
    println(conference.averageFinalScore(4))
    println(conference.acceptedArticles())
    println(conference.sortedAcceptedArticles())
    println(conference.averageWeightedFinalScore())