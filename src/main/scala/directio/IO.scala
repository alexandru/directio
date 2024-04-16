package directio

import language.experimental.erasedDefinitions
import language.experimental.saferExceptions
import directio.Async.unsafe

erased trait Sync
object Sync:
    object unsafe:
        given Sync = compiletime.erasedValue

erased trait Async extends Sync
object Async:
    object unsafe:
        given Async = compiletime.erasedValue

type NonBlocking[+T] = Sync ?=> T
object NonBlocking:
    def run[T](f: NonBlocking[T]): T =
        import Sync.unsafe.given
        f

type Blocking[+T] = (Async, CanThrow[InterruptedException]) ?=> T
object Blocking:
    inline def run[T](inline f: Blocking[T]): T throws InterruptedException =
        import Async.unsafe.given
        f

    inline def runUnsafeExceptions[T](inline f: Blocking[T]): T =
        import unsafeExceptions.given
        run(f)
