package directio

import scala.util.control.NonFatal
import java.util.concurrent.atomic.AtomicLong

opaque type FiberId <: Long = Long
object FiberId:
    private val idRef = new AtomicLong(0)

    def apply(id: Long): FiberId = id

    def newId(): NonBlocking[FiberId] =
        apply(idRef.getAndIncrement())

trait Fiber[+A] extends Cancellable:
    def id: FiberId
    def join(): Blocking[Outcome[A]]

    def cancelAndJoin(): Blocking[Outcome[A]] =
        cancel()
        join()

object Fiber:

    def completed[A](outcome: Outcome[A]): NonBlocking[Fiber[A]] =
        new Fiber[A]:
            val id = FiberId.newId()
            def cancel(): Blocking[Unit] = ()
            def join(): Blocking[Outcome[A]] =
                outcome

    def fromCancellable(ref: Cancellable): NonBlocking[Fiber[Unit]] =
        new Fiber[Unit]:
            private val completed = Deferred[Unit]()
            val id = FiberId.newId()

            def cancel(): Blocking[Unit] =
                var errorThrownByCancel = true
                try
                    ref.cancel()
                    errorThrownByCancel = false
                    completed.complete(Outcome.Cancelled(InterruptedException()))
                catch
                    case NonFatal(e) if errorThrownByCancel =>
                        completed.complete(Outcome.Failure(e))
                    case e: InterruptedException if errorThrownByCancel =>
                        completed.complete(Outcome.Cancelled(e))

            def join(): Blocking[Outcome[Unit]] =
                completed.awaitComplete()
end Fiber
