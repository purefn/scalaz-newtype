package scalaz
package newtype.deriving
package example

import scalaz.effect._

package MonadTransformerNewtype {
  final case class WibbleT[M[_], A](run: Kleisli[M, String, A])

  object WibbleT extends WibbleTInstances.Instances

  @deriving[M[_]](Functor[WibbleT[M, ?]])(Functor[M])
  @deriving[M[_]](Monad[WibbleT[M, ?]])(Monad[M])
  @deriving[M[_]](MonadIO[WibbleT[M, ?]])(MonadIO[M])
  @deriving[M[_]](MonadCatchIO[WibbleT[M, ?]])(MonadCatchIO[M])
  object WibbleTInstances

  final case class WobbleT[M[_], A](run: Kleisli[M, Int, A])

  object WobbleT extends WobbleTInstances.Instances

  @deriving[M[_]](Functor[WobbleT[M, ?]])(Functor[M])
  @deriving[M[_]](Monad[WobbleT[M, ?]])(Monad[M])
  @deriving[M[_]](MonadIO[WobbleT[M, ?]])(MonadIO[M])
  @deriving[M[_]](MonadCatchIO[WobbleT[M, ?]])(MonadCatchIO[M])
  object WobbleTInstances

  object Stack {
    type Thing2[A] = IO[A]
    type Thing1[A] = WibbleT[Thing2, A]
  }

  final case class Thing[A](run: WobbleT[Stack.Thing1, A])

  object Thing extends ThingInstances.Instances

  @deriving(Functor[Thing])
  @deriving(Monad[Thing])
  @deriving(MonadIO[Thing])
  @deriving(MonadCatchIO[Thing])
  object ThingInstances

  object TestThing {
    Functor[Thing]
    Monad[Thing]
    MonadIO[Thing]
    MonadCatchIO[Thing]
  }
}

