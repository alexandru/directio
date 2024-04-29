package directio

import directio.platform.*

type Poll[A] = Blocking[A] => NonBlocking[A]

trait Async extends Sync:
    def createUncancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]]
    def createCancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]]

    def cede: Blocking[Unit]

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
