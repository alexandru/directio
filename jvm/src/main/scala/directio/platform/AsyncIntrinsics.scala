package directio
package platform

private[directio] class AsyncIntrinsics extends SyncIntrinsics with Async:
    private given VirtualThreadBuilder = VirtualThreadBuilder()(using this)

    def cede: Blocking[Unit] =
        Thread.onSpinWait()
        if Thread.interrupted() then
            throw InterruptedException()

    def createUncancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]] =
        JvmFiber.uncancellable(block)

    def createCancellableFiber[A](block: Fiber[A] ?=> Blocking[A]): NonBlocking[Fiber[A]] =
        JvmFiber.cancellable(block)

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A] =
        directio.platform.uncancellable(block)

    def cont[A](block: Continuation[A] => Blocking[A]): Blocking[A] = ???
    // directio.platform.cont(block)
