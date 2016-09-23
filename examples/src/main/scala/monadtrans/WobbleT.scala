package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._
import scalaz.newtype.deriving._

final case class WobbleT[M[_], A](run: Kleisli[M, Int, A])

object WobbleT extends WobbleTInstances.Instances

@deriving[M[_]](Functor[WobbleT[M, ?]])(Functor[M])
@deriving[M[_]](Monad[WobbleT[M, ?]])(Monad[M])
@deriving[M[_]](MonadIO[WobbleT[M, ?]])(MonadIO[M])
@deriving[M[_]](MonadCatchIO[WobbleT[M, ?]])(MonadCatchIO[M])
object WobbleTInstances

