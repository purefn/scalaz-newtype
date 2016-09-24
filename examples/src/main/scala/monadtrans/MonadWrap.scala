package scalaz.newtype.deriving.examples.monadtrans

import scalaz._
import scalaz.effect._

trait MonadWrap[M[_]] {
  def wrap(m: M[Unit]): M[Unit]
}

object MonadWrap {

  implicit def KleisliMonadWrap[M[_], R](implicit M: MonadWrap[M]): MonadWrap[Kleisli[M, R, ?]] =
    new MonadWrap[Kleisli[M, R, ?]] {
      def wrap(k: Kleisli[M, R, Unit]): Kleisli[M, R, Unit] =
        Kleisli(r => M.wrap(k.run(r)))
    }

  implicit def IOMonadWrap: MonadWrap[IO] =
    new MonadWrap[IO] {
      def wrap(io: IO[Unit]): IO[Unit] =
        for {
          _ <- IO.putStrLn("wrapping...")
          _ <- io
          _ <- IO.putStrLn("done")
        } yield ()
    }
}

