package directio
package platform

private[platform] trait JvmFiber[+A] extends Fiber[A]:
    def start(): NonBlocking[Unit]

private[platform] object JvmFiber:
    val currentFiber: ThreadLocal[Fiber[_] | Null] =
        ThreadLocal.withInitial(() => null)

    inline def withCurrentFiber[A](fiber: Fiber[?])(
        inline block: Blocking[A]
    ): Blocking[A] =
        currentFiber.set(fiber)
        try
            block
        finally
            currentFiber.set(null)

    def cancellable[A](
        block: Fiber[A] ?=> Blocking[A]
    )(using VirtualThreadBuilder): NonBlocking[Fiber[A]] =
        new JvmFiber[A]:
            self =>
            val id = FiberId.newId()

            private val cancelRef = Ref[InterruptedException | Null](null)
            private val deferred = Deferred[A]()

            private val th = summon[VirtualThreadBuilder].unstarted(() =>
                Blocking.run:
                    deferred.completeWith:
                        try
                            withCurrentFiber(self):
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

    def uncancellable[A](
        block: Fiber[A] ?=> Blocking[A]
    )(using VirtualThreadBuilder): NonBlocking[Fiber[A]] =
        new JvmFiber[A]:
            self =>
            val id = FiberId.newId()
            private val deferred = Deferred[A]()
            private val thread = summon[VirtualThreadBuilder].unstarted(() =>
                Blocking.run:
                    deferred.completeWith:
                        withCurrentFiber(self):
                            block(using self)
            )
            def isActive = true
            def start() = thread.start()
            def outcome = deferred.outcome
            def cancel(e: InterruptedException | Null): NonBlocking[Unit] = ()
            def join() = deferred.join()
end JvmFiber
