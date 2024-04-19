package directio

import scala.annotation.implicitNotFound

/** Capability/evidence required for non-blocking operations.
  *
  * @see
  *   [[NonBlocking]]
  */
@implicitNotFound(
    "`Sync` given is required. This operation requires a non-blocking context. " +
        "Either mark the function's return type as `directio.NonBlocking`, or use " +
        "`directio.NonBlocking.run` to execute the function."
)
sealed trait Sync
object Sync:
    object unsafe:
        given Sync = new {}

type NonBlocking[+T] = Sync ?=> T
object NonBlocking:
    def run[T](f: NonBlocking[T]): T =
        import Sync.unsafe.given
        f

sealed trait Async extends Sync
object Async:
    object unsafe:
        given Async = new {}

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
