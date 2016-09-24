package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._
import scalaz.newtype.deriving._

final case class Other[A](run: Thing0[A])

object Other extends OtherInstances.Instances

@deriving(Functor[Other])
@deriving(Monad[Other])
@deriving(MonadIO[Other])
@deriving(MonadCatchIO[Other])
object OtherInstances

object TestOther {
  Functor[Other]
  Monad[Other]
  MonadIO[Other]
  MonadCatchIO[Other]
}

