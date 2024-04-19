package directio

import directio.util.*
import scala.collection.immutable.Queue

final class Deferred[A](
    ref: Ref[Outcome[A] | Queue[Callback[A]]]
)(using FailureReporter):
    private def unregister(cb: Callback[A]): NonBlocking[Unit] =
        ref.update:
            case queue: Queue[Callback[A]] => queue.filterNot(_ == cb)
            case other => other

    def awaitComplete(): Blocking[Outcome[A]] =
        val cbRef = BlockingCallback[A]()
        val token = onComplete(cbRef)
        try
            cbRef.await()
        catch
            case e: InterruptedException =>
                token.cancel()
                throw e

    def onComplete(cb: Callback[A]): NonBlocking[Cancellable] =
        ref.modify:
            case o: Outcome[A] =>
                (o, Left(o))
            case queue: Queue[Callback[A]] =>
                (queue.enqueue(cb), Right(Cancellable.idempotent(unregister(cb))))
        match
            case Left(value) =>
                cb(value)
                Cancellable.empty
            case Right(cancellable) =>
                cancellable

    def complete(outcome: Outcome[A]): Blocking[Unit] =
        val queue = ref.modify:
            case queue: Queue[Callback[A]] =>
                (outcome, queue)
            case _: Outcome[A] =>
                throw new IllegalStateException("Promise already completed")
        for cb <- queue do
            logAndIgnoreExceptions(cb(outcome))

    def completeWith(body: Blocking[A]): Blocking[Unit] =
        val r =
            try
                Outcome.Success(body)
            catch
                case e: InterruptedException =>
                    Outcome.Cancelled(e)
                case e: Throwable =>
                    Outcome.Failure(e)
        complete(r)
end Deferred

object Deferred:
    def apply[A]()(using FailureReporter): NonBlocking[Deferred[A]] =
        new Deferred(Ref(Queue.empty))
