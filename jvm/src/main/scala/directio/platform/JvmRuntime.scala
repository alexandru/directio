package directio
package platform

private[directio] class JvmRuntime extends Runtime:
    private given Runtime = this

    def reportFailure(cause: Throwable): NonBlocking[Unit] =
        val th = Thread.currentThread()
        th.getUncaughtExceptionHandler().uncaughtException(th, cause)

    private def cancelThread(th: Thread): Blocking[Unit] =
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

    def start[T](block: Blocking[T]): NonBlocking[Fiber[T]] =
        val deferred = Deferred[T]()
        val th = Thread.ofVirtual().unstarted(() =>
            Blocking.run:
                deferred.completeWith(block)
        )
        val fiber = new Fiber[T]:
            def join(): Blocking[Outcome[T]] =
                deferred.awaitComplete()

            def cancel(): Blocking[Unit] =
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

        th.start()
        fiber

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A] =
        val cancel = MultiAssignCancellable()

        def poll(id: Long): Poll[A] = block =>
            val th = Thread.currentThread()
            if th.threadId() != id then
                throw IllegalStateException("Poll reference leaked to a different fiber/thread")

            if cancel.isCancelled then throw InterruptedException()
            cancel.set(Cancellable:
                if th == Thread.currentThread() then
                    throw InterruptedException()
                cancelThread(th)
            )
            try
                block
            finally
                cancel.clear()

        val fiber = start:
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
                    wasCancelled = true
            if wasCancelled then
                cancel.cancel()

        ret.getOrThrow


private[directio] trait RuntimeCompanionPerPlatform:
    def global: Runtime = JvmRuntime()
