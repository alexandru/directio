package directio

import java.util.concurrent.atomic.AtomicBoolean

trait Cancellable:
    def cancel(): Blocking[Unit]

object Cancellable:
    inline def apply(inline f: Blocking[Unit]): NonBlocking[Cancellable] =
        new Cancellable:
            def cancel() = f

    inline def idempotent(inline f: Blocking[Unit]): NonBlocking[Cancellable] =
        new Cancellable:
            private val cancelled = AtomicBoolean(false)
            def cancel() =
                if !cancelled.getAndSet(true) then f

    val empty: Cancellable =
        new Cancellable:
            def cancel() = ()

final class MultiAssignCancellable(ref: Ref[Null | Cancellable | Unit])
    extends Cancellable:

    def isCancelled: NonBlocking[Boolean] =
        ref.get match
            case () => true
            case null | _: Cancellable => false

    def clear(): NonBlocking[Unit] =
        ref.update:
            case _: Cancellable | null => null
            case () => ()

    def set(value: Cancellable): Blocking[Unit] =
        ref.modify:
            case null | _: Cancellable => (value, null)
            case () => ((), value)
        match
            case null => ()
            case c: Cancellable => c.cancel()

    def cancel(): Blocking[Unit] =
        ref.getAndSet(()) match
            case null | () => ()
            case c: Cancellable => c.cancel()

object MultiAssignCancellable:
    def apply(): NonBlocking[MultiAssignCancellable] =
        new MultiAssignCancellable(Ref(null))
