package directio
package platform

import scala.util.control.NonFatal

class AsyncIntrinsics extends SyncIntrinsics with Async:
    private def cancelThread(th: Thread): Blocking[Unit] =
        // Are we cancelling the fiber from within itself?
        if Thread.currentThread().threadId() == th.threadId() then
            throw InterruptedException()

        var wasInterrupted = false
        while th.isAlive() do
            try
                th.interrupt()
                th.join(200)
                wasInterrupted ||= Thread.interrupted()
            catch
                case e: InterruptedException =>
                    wasInterrupted = true
        if wasInterrupted then
            throw InterruptedException()

    def forkUnsafe[T](block: FiberId => Blocking[T]): NonBlocking[Fiber[T]] =
        val deferred = Deferred[T]()
        val fiberId = FiberId.newId()
        val th = Thread.ofVirtual().unstarted(() =>
            Blocking.run:
                deferred.completeWith(block(fiberId))
        )
        val fiber = new Fiber[T]:
            val id = fiberId

            def join(): Blocking[Outcome[T]] =
                deferred.awaitComplete()

            def cancel(): Blocking[Unit] =
                cancelThread(th)

        th.start()
        fiber

    def guaranteeCase[T](block: Blocking[T])(finalizer: Outcome[T] => Blocking[Unit]): Blocking[T] =
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

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A] =
        val cancel = MultiAssignCancellable()

        def poll(threadId: Long): Poll[A] = block =>
            val th = Thread.currentThread()
            if th.threadId() != threadId then
                throw IllegalStateException("Poll reference leaked to a different fiber/thread")

            if cancel.isCancelled then throw InterruptedException()
            cancel.set(Cancellable:
                cancelThread(th)
            )
            try
                block
            finally
                cancel.clear()

        val fiber = forkUnsafe: _ =>
            val id = Thread.currentThread().threadId()
            block(poll(id))

        var wasCancelled = false
        var ret: Outcome[A] | Null = null

        while ret == null do
            try
                ret = fiber.join()
                wasCancelled ||= Thread.interrupted()
            catch
                case e: InterruptedException =>
                    Thread.interrupted()
                    wasCancelled = true
            if wasCancelled then
                cancel.cancel()

        ret.getOrThrow
    end uncancellable

end AsyncIntrinsics
