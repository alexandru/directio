package directio

trait Job[+A]:
    def isActive: NonBlocking[Boolean]
    def cancel(exception: InterruptedException | Null): NonBlocking[Unit]
    def join(): Blocking[Unit]
    def outcome: NonBlocking[Outcome[A] | Null]

    final def awaitOutcome(): Blocking[Outcome[A]] =
        join()
        outcome.nn

    final def cancel(): NonBlocking[Unit] =
        cancel(null)

    final def cancelAndJoin(): Blocking[Unit] =
        cancelAndJoin(null)

    final def cancelAndJoin(exception: InterruptedException | Null): Blocking[Unit] =
        cancel(exception)
        join()

    final def cancelAndAwaitOutcome(): Blocking[Outcome[A]] =
        cancelAndAwaitOutcome(null)

    final def cancelAndAwaitOutcome(exception: InterruptedException | Null): Blocking[Outcome[A]] =
        cancelAndJoin(exception)
        outcome.nn
end Job

object Job:
    def completed[A](value: Outcome[A]): Job[A] =
        new Job[A]:
            def isActive = false
            def cancel(exception: InterruptedException | Null): NonBlocking[Unit] = ()
            def join(): Blocking[Unit] = ()
            def outcome: NonBlocking[Outcome[A] | Null] = value
