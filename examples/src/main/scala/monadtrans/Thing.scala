package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._
import scalaz.newtype.deriving._

final case class Thing[A](run: WobbleT[Thing1, A])

object Thing extends ThingInstances.Instances

@deriving(Functor[Thing])
@deriving(Monad[Thing])
@deriving(MonadIO[Thing])
@deriving(MonadCatchIO[Thing])
@deriving(MonadWrap[Thing])
object ThingInstances

object TestThing {
  Functor[Thing]
  Monad[Thing]
  MonadIO[Thing]
  MonadCatchIO[Thing]
}

