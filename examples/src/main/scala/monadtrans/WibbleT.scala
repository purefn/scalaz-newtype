package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._
import scalaz.newtype.deriving._

final case class WibbleT[M[_], A](run: Kleisli[M, String, A])

object WibbleT extends WibbleTInstances.Instances

@deriving[M[_]](Functor[WibbleT[M, ?]])(Functor[M])
@deriving[M[_]](Monad[WibbleT[M, ?]])(Monad[M])
@deriving[M[_]](MonadIO[WibbleT[M, ?]])(MonadIO[M])
@deriving[M[_]](MonadCatchIO[WibbleT[M, ?]])(MonadCatchIO[M])
object WibbleTInstances

