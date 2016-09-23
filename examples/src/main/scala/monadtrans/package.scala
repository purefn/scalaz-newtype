package scalaz.newtype.deriving.examples

import scalaz.effect._

package object monadtrans {
  type Thing2[A] = IO[A]
  type Thing1[A] = WibbleT[Thing2, A]
}

