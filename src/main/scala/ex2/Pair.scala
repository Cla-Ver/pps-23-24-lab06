package ex2

trait Pair[A, B]:
  def getX(): A
  def getY(): B

object Pair:
    def apply[A, B](a: A, b: B): Pair[A, B] = PairImpl(a, b)

private case class PairImpl[A, B](a: A, b: B) extends Pair[A, B]:
    def getX(): A = a
    def getY(): B = b
    override def toString(): String = "(" + a + ", " + b + ")"