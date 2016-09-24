package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._

trait MonadWrap[M[_]] {
  def wrap(m: M[Unit]): M[Unit]

  // test for an edge-case where using the decoded type name
  // resulted in a compiler error saying
  //
  //   \/ is not a member of scalaz
  def or(a: String \/ String): M[Unit]
}

object MonadWrap {

  implicit def KleisliMonadWrap[M[_], R](implicit M: MonadWrap[M]): MonadWrap[Kleisli[M, R, ?]] =
    new MonadWrap[Kleisli[M, R, ?]] {
      override def wrap(k: Kleisli[M, R, Unit]): Kleisli[M, R, Unit] =
        Kleisli(r => M.wrap(k.run(r)))

      override def or(a: String \/ String) = ???
    }

  implicit def IOMonadWrap: MonadWrap[IO] =
    new MonadWrap[IO] {
      override def wrap(io: IO[Unit]): IO[Unit] =
        for {
          _ <- IO.putStrLn("wrapping...")
          _ <- io
          _ <- IO.putStrLn("done")
        } yield ()

      override def or(a: String \/ String) = ???
    }
}

