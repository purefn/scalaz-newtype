package scalaz
package newtype.deriving
package example

import scalaz.effect._

package SimplePolymorphicNewtype {
  final case class Thing[A](run: IO[A])

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

