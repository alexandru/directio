package directio

import directio.platform.*

type Poll[A] = Blocking[A] => NonBlocking[A]

trait Async extends Sync:
    def forkUnsafe[A](block: FiberId => Blocking[A]): NonBlocking[Fiber[A]]

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A]

    def guaranteeCase[A](block: Blocking[A])(finalizer: Outcome[A] => Blocking[Unit]): Blocking[A]

object Async:
    object unsafe:
        given Async = new AsyncIntrinsics()

type Blocking[+A] = Async ?=> A
object Blocking:
    @throws[InterruptedException]
    inline def run[A](inline f: Blocking[A]): A =
        import Async.unsafe.given
        f

opaque type IO[+A] = () => Blocking[A]
object IO:
    inline def apply[A](inline block: Blocking[A]): IO[A] =
        () => block
