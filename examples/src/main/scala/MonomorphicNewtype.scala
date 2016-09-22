package scalaz
package newtype.deriving
package example

import scalaz._
import Scalaz._

package MonomorphicNewtype {
  final case class PosInt(n: Option[Int])

  object PosInt extends PosIntInstances.SInstances

  trait OtherInstances

  @deriving(Semigroup[PosInt])
  @deriving(Monoid[PosInt])
  object PosIntInstances {
    trait SInstances extends OtherInstances
  }

  object TestPosInt {
    Semigroup[PosInt]
    Monoid[PosInt]
  }
}

