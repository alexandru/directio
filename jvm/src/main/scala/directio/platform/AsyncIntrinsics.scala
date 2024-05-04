package directio
package platform

import scala.util.control.NonFatal

private[platform] trait JvmFiber[+A] extends Fiber[A]:
    def start(): NonBlocking[Unit]

private[directio] class AsyncIntrinsics extends SyncIntrinsics with Async:

    private val threadFactory: Thread.Builder =
        Thread.ofVirtual()
            .name("directio-fiber")
            .uncaughtExceptionHandler((_, e) =>
                NonBlocking.run(reportFailure(e))
            )

    // private def cancelThreadMany(th: Thread): Blocking[Unit] =
    //     // Are we cancelling the fiber from within itself?
    //     if Thread.currentThread().threadId() == th.threadId() then
    //         throw InterruptedException()
    //
    //     var wasInterrupted = false
    //     while th.isAlive() do
    //         try
    //             th.interrupt()
    //             th.join(200)
    //             wasInterrupted ||= Thread.interrupted()
    //         catch
    //             case e: InterruptedException =>
    //                 wasInterrupted = true
    //     if wasInterrupted then
    //         throw InterruptedException()

    def cede: Blocking[Unit] =
        Thread.onSpinWait()
        if Thread.interrupted() then
            throw InterruptedException()

    def createUncancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]] =
        new JvmFiber[A]:
            self =>
            val id = FiberId.newId()
            private val deferred = Deferred[A]()
            private val thread = threadFactory.unstarted(() =>
                Blocking.run:
                    deferred.completeWith(block(using self))
            )
            def isActive = true
            def start() = thread.start()
            def outcome = deferred.outcome
            def cancel(e: InterruptedException | Null): NonBlocking[Unit] = ()
            def join() = deferred.join()

    def createCancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]] =
        new JvmFiber[A]:
            self =>
            val id = FiberId.newId()

            private val cancelRef = Ref[InterruptedException | Null](null)
            private val deferred = Deferred[A]()

            private val th = Thread.ofVirtual().unstarted(() =>
                Blocking.run:
                    deferred.completeWith:
                        try
                            block(using self)
                        catch
                            case e: InterruptedException =>
                                cancelRef.get match
                                    case null => throw e
                                    case e2 =>
                                        e2.addSuppressed(e)
                                        throw e2
            )

            def start() = th.start()
            def isActive = cancelRef.get == null
            def outcome = deferred.outcome
            def join() = deferred.join()

            def cancel(e: InterruptedException | Null): NonBlocking[Unit] =
                val err = e match
                    case null => InterruptedException()
                    case e => e
                if cancelRef.compareAndSet(null, err) then
                    th.interrupt()

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

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A] = ???
    //     val cancel = MultiAssignCancellable()

    //     def poll(threadId: Long): Poll[A] = block =>
    //         val th = Thread.currentThread()
    //         if th.threadId() != threadId then
    //             throw IllegalStateException("Poll reference leaked to a different fiber/thread")

    //         if cancel.isCancelled then throw InterruptedException()
    //         cancel.set(Cancellable:
    //             th.interrupt()
    //         )
    //         try
    //             block
    //         finally
    //             cancel.clear()

    //     val fiber = startInterruptible:
    //         val id = Thread.currentThread().threadId()
    //         block(poll(id))

    //     var wasCancelled = false
    //     var ret: Outcome[A] | Null = null

    //     while ret == null do
    //         try
    //             fiber.join()
    //             ret = fiber.outcome.nn
    //             wasCancelled ||= Thread.interrupted()
    //         catch
    //             case e: InterruptedException =>
    //                 Thread.interrupted()
    //                 wasCancelled = true
    //         if wasCancelled then
    //             cancel.cancel()

    //     ret.getOrThrow
    // end uncancellable

end AsyncIntrinsics
