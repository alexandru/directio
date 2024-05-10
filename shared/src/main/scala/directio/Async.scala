package directio

import directio.platform.*
import scala.util.control.NonFatal

type Poll[A] = Blocking[A] => NonBlocking[A]

trait Async extends Sync:
    def cede: Blocking[Unit]
    def cont[A](block: Continuation[A] => Blocking[A]): Blocking[A]
    def createCancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]]
    def createUncancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]]
    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A]

    def guaranteeCase[A](block: Blocking[A])(finalizer: Outcome[A] => Blocking[Unit]): Blocking[A] =
        var isFinalizerException = false
        try
            val ret = block
            isFinalizerException = true
            finalizer(Outcome.Success(ret))
            ret
        catch
            case NonFatal(e) if !isFinalizerException =>
                try
                    finalizer(Outcome.Failure(e))
                catch
                    case NonFatal(e2) =>
                        e.addSuppressed(e2)
                throw e
            case e: InterruptedException if !isFinalizerException =>
                try
                    finalizer(Outcome.Cancelled(e))
                catch
                    case NonFatal(e2) =>
                        e.addSuppressed(e2)
                throw e
end Async

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
