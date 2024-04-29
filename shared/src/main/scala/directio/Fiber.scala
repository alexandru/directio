package directio

import java.util.concurrent.atomic.AtomicLong

opaque type FiberId <: Long = Long
object FiberId:
    private val idRef = new AtomicLong(0)

    def apply(id: Long): FiberId = id

    def newId(): NonBlocking[FiberId] =
        apply(idRef.getAndIncrement())

trait Fiber[+A] extends Job[A]:
    def id: FiberId
    def start(): NonBlocking[Unit]

object Fiber:
    def fromJob[A](job: Job[A]): NonBlocking[Fiber[A]] =
        job match
            case ref: Fiber[A] => ref
            case _ =>
                new Fiber[A]:
                    val id = FiberId.newId()
                    def start(): NonBlocking[Unit] = ()
                    def cancel(exception: InterruptedException | Null): NonBlocking[Unit] =
                        job.cancel(exception)
                    def join(): Blocking[Unit] =
                        job.join()
                    def outcome: NonBlocking[Outcome[A] | Null] =
                        job.outcome
                    def isActive: NonBlocking[Boolean] =
                        job.isActive

    def completed[A](value: Outcome[A]): NonBlocking[Fiber[A]] =
        new Fiber[A]:
            val id = FiberId.newId()
            def start() = ()
            def cancel(exception: InterruptedException | Null): NonBlocking[Unit] = ()
            def join(): Blocking[Unit] = ()
            def outcome: NonBlocking[Outcome[A] | Null] = value
            def isActive: NonBlocking[Boolean] = false

    // def fromCancellable(ref: Cancellable): NonBlocking[Fiber[Unit]] =
    //     new Fiber[Unit]:
    //         private val completed = Deferred[Unit]()
    //         val id = FiberId.newId()

    //         def cancel(): Blocking[Unit] =
    //             var errorThrownByCancel = true
    //             try
    //                 ref.cancel()
    //                 errorThrownByCancel = false
    //                 completed.complete(Outcome.Cancelled(InterruptedException()))
    //             catch
    //                 case NonFatal(e) if errorThrownByCancel =>
    //                     completed.complete(Outcome.Failure(e))
    //                 case e: InterruptedException if errorThrownByCancel =>
    //                     completed.complete(Outcome.Cancelled(e))

    //         def join(): Blocking[Outcome[Unit]] =
    //             completed.awaitComplete()
end Fiber
