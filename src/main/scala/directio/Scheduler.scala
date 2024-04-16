package directio

import scala.concurrent.duration.FiniteDuration

trait Dispatcher:
    def reportError(e: Throwable): NonBlocking[Unit]

    def scheduleOnce(
        delay: FiniteDuration,
        mayInterruptIfRunning: Boolean = true
    )(r: Runnable): NonBlocking[Cancellable]

object Dispatcher:
    object Virtual extends Dispatcher:
        private lazy val scheduledExecutor =
            java.util.concurrent.Executors.newScheduledThreadPool(1)

        def reportError(e: Throwable): NonBlocking[Unit] =
            val th = Thread.currentThread()
            th.getUncaughtExceptionHandler().uncaughtException(th, e)

        def scheduleOnce(
            delay: FiniteDuration,
            mayInterruptIfRunning: Boolean = true
        )(r: Runnable): NonBlocking[Cancellable] =
            val cancel = MultiAssignCancellable()
            val th = Thread.ofVirtual().unstarted(() =>
                Blocking.run:
                    val th = Thread.currentThread()
                    if mayInterruptIfRunning then
                        cancel.set(Cancellable:
                            th.interrupt()
                            th.join()
                        )
                if !Thread.interrupted() then
                    r.run()
            )
            val token = scheduledExecutor.schedule(
                (() => th.start()): Runnable,
                delay.length,
                delay.unit
            )
            Cancellable:
                token.cancel(mayInterruptIfRunning)
                cancel.cancel()
    end Virtual
end Dispatcher
