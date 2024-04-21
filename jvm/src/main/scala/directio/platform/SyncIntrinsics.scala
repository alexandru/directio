package directio
package platform

private[directio] class SyncIntrinsics extends Sync:
    def reportFailure(cause: Throwable): NonBlocking[Unit] =
        val th = Thread.currentThread()
        th.getUncaughtExceptionHandler().uncaughtException(th, cause)
