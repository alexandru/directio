package directio

import scala.language.experimental.erasedDefinitions
import language.experimental.saferExceptions
import java.io.IOException

erased trait Sync
object Sync:
    object unsafe:
        given Sync = compiletime.erasedValue

type SyncIO[+T] = Sync ?=> T
object SyncIO:
    inline def apply[T](inline block: SyncIO[T]): SyncIO[T] = block

erased trait Async extends CanThrow[InterruptedException | IOException] with Sync
object Async:
    object unsafe:
        given Async = compiletime.erasedValue

type AsyncIO[+T] = Async ?=> T
object AsyncIO:
    inline def apply[T](inline block: AsyncIO[T]): AsyncIO[T] = block
