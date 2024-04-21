package directio

import scala.annotation.implicitNotFound
import directio.platform.*

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
trait Sync:
    def reportFailure(cause: Throwable): NonBlocking[Unit]

object Sync:
    object unsafe:
        given Sync = new SyncIntrinsics()

type NonBlocking[+T] = Sync ?=> T
object NonBlocking:
    def run[T](f: NonBlocking[T]): T =
        import Sync.unsafe.given
        f
