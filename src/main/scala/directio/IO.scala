package directio

import directio.Async.unsafe

sealed trait Sync
object Sync:
    object unsafe:
        given Sync = new {}

sealed trait Async extends Sync
object Async:
    object unsafe:
        given Async = new {}

type NonBlocking[+T] = Sync ?=> T
object NonBlocking:
    def run[T](f: NonBlocking[T]): T =
        import Sync.unsafe.given
        f

type Blocking[+T] = Async ?=> T
object Blocking:
    @throws[InterruptedException]
    inline def run[T](inline f: Blocking[T]): T =
        import Async.unsafe.given
        f

opaque type IO[+T] = () => Blocking[T]
object IO:
    inline def apply[T](inline block: Blocking[T]): IO[T] =
        () => block
