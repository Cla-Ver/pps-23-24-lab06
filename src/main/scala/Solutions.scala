// ==== Task 1 ====

package ex1

import scala.collection.View.Collect

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = foldLeft(Nil())((b, a) => b.append(List((a, value))))
  
  def length(): Int = foldLeft(0)((a, b) => a + 1)
  def zipWithIndex: List[(A, Int)] = foldLeft(Nil())((b, a) => b.append(List((a, b.length()))))
  def partition(predicate: A => Boolean): (List[A], List[A]) = foldLeft((Nil(), Nil()))((b, a) => if predicate(a) then (b._1.append(List(a)), b._2) else (b._1, b._2.append(List(a))))
  def span(predicate: A => Boolean): (List[A], List[A]) = foldLeft((Nil(), Nil()))((b, a) => if predicate(a) && b._2.length() == 0 then (b._1.append(List(a)), b._2) else (b._1, b._2.append(List(a))))
  def takeRight(n: Int): List[A] = reverse(foldRight(Nil())((a, b) => if b.length() < n then b.append(List(a)) else b.append(Nil())))
  def collect(predicate: PartialFunction[A, A]): List[A] = foldLeft(Nil())((b, a) => if predicate.isDefinedAt(a) then b.append(List(predicate(a))) else b.append(Nil()))
  private def reverse(list: List[A]): List[A] = list.foldRight(Nil())((a, b) => b.append(List(a)))
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

// ==== Task 2 ====

trait Pair[A, B]:
  def getX(): A
  def getY(): B

object Pair:
    def apply[A, B](a: A, b: B): Pair[A, B] = PairImpl(a, b)

private case class PairImpl[A, B](a: A, b: B) extends Pair[A, B]:
    def getX(): A = a
    def getY(): B = b
    override def toString(): String = "(" + a + ", " + b + ")"

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
  def orderedScores(article: Int, question: Question): scala.List[Int]
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): scala.List[Pair[Int, Double]]
  def averageWeightedFinalScore(): Map[Int, Double]
  def numberOfReviews(): Int


object ConferenceReviewing:
    def apply(reviews: scala.List[Review]): ConferenceReviewing = ConferenceReviewingImpl()

private class ConferenceReviewingImpl(reviews: scala.List[Pair[Int, Map[Question, Int]]] = Nil) extends ConferenceReviewing():

    private var this.reviews: scala.List[Pair[Int, Map[Question, Int]]] = reviews

    override def numberOfReviews(): Int = this.reviews.length
    override def loadReview(review: Review): ConferenceReviewing = ConferenceReviewingImpl(this.reviews concat scala.List(Pair(review.article, Map(Question.Relevance -> review.relevance, Question.Significance -> review. significance, Question.Confidence -> review.confidence, Question.Final -> review.finalScore))))
    override def orderedScores(article: Int, question: Question): scala.List[Int] = this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(question).get).sorted
    override def averageWeightedFinalScore(): Map[Int, Double] = this.reviews.map(p => p.getX()).distinct.map(p => (p, singleArticleWeightedFinalScore(p))).toMap
    override def acceptedArticles(): Set[Int] = this.reviews.map(p => p.getX()).distinct.filter(p => accepted(p)).toSet
    override def sortedAcceptedArticles(): scala.List[Pair[Int, Double]] = acceptedArticles().map(p => Pair(p, averageFinalScore(p))).toList.sorted((e1, e2) => e1.getY().compareTo(e2.getY()))
    override def averageFinalScore(article: Int): Double = mean(this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Final).orElse(Some(0)).get))
    
    private def mean(list: scala.List[Int]): Double = list.sum.toDouble / list.length
    private def accepted(article: Int): Boolean = averageFinalScore(article) >= 5.0 && this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Relevance)).max.orElse(Some(0)).get >= 8.0
    private def singleArticleWeightedFinalScore(article: Int): Double = mean(this.reviews.filter(p => p.getX() == article).map(p => p.getY().get(Question.Relevance).get * p.getY().get(Question.Final).get / 10))