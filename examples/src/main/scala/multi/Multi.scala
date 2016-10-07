package scalaz.newtype.deriving.examples.multi

import scalaz.newtype.deriving._

trait MonadReader[M[_], A] {
  def ask: M[A]
  def local[B](f: A => A)(action: M[B]): M[B]
}

object MonadReader {
  implicit def FunctionMonadReader[A]: MonadReader[A => ?, A] =
    new MonadReader[A => ?, A] {
      def ask: A => A = identity
      def local[B](f: A => A)(action: A => B): A => B =
        a => action(f(a))
    }
}

// deriving without a type alias
final case class StringReader[B](run: String => B)

object StringReader extends StringReaders.Instances

@deriving(MonadReader[StringReader, String])
object StringReaders

// deriving with a type alias
final case class IntReader[B](run: Int => B)

object IntReader extends IntReaders.Instances

package object types {
  type MonadIntReader[M[_]] = MonadReader[M, Int]
}
import types._

@deriving(MonadIntReader[IntReader])
object IntReaders

object Test {
  // the instance name for the aliased version should use the type alias name,
  // not the name of the thing being aliased
  val _ = IntReader.IntReaderMonadIntReader
}
