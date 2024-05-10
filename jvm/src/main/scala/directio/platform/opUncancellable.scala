package directio
package platform

import scala.util.control.NonFatal

private[platform] def uncancellable[A](
    block: Poll[A] => Blocking[A]
)(using VirtualThreadBuilder): Blocking[A] =
    val cancel = MaskedCancellable()

    def poll(using Fiber[A]): Poll[A] = block =>
        JvmFiber.currentFiber.get() match
            case null =>
                throw IllegalStateException("Poll reference leaked, no longer in a fiber")
            case fb: Fiber[?] =>
                if fb.id != summon[Fiber[A]].id then
                    throw IllegalStateException("Poll reference leaked to a different fiber")

        if cancel.isCancelled then throw InterruptedException()
        var result: Outcome[A] | Null = null
        val th = summon[VirtualThreadBuilder].unstarted(() =>
            Blocking.run:
                JvmFiber.withCurrentFiber(summon[Fiber[A]]):
                    try
                        result = Outcome.Success(block)
                    catch
                        case e: InterruptedException =>
                            result = Outcome.Cancelled(e)
                        case NonFatal(e) =>
                            result = Outcome.Failure(e)
        )
        cancel.withCancellableThread(th):
            th.start()
            var wasCancelled = false
            while th.isAlive() do
                try
                    th.join()
                catch
                    case e: InterruptedException =>
                        if !wasCancelled then
                            wasCancelled = true
                            cancel.cancel()

            wasCancelled ||= Thread.interrupted()
            if wasCancelled then
                throw InterruptedException()
            else
                result match
                    case null => throw InterruptedException()
                    case res => res.getOrThrow

    val fiber = JvmFiber.cancellable(block(poll))
    fiber.start()

    var wasCancelled = false
    var ret: Outcome[A] | Null = null
    while ret == null do
        try
            fiber.join()
            ret = fiber.outcome.nn
            wasCancelled ||= Thread.interrupted()
        catch
            case e: InterruptedException =>
                Thread.interrupted()
                wasCancelled = true
        if wasCancelled then
            cancel.cancel()

    ret.getOrThrow
end uncancellable

private final class MaskedCancellable private (
    ref: Ref[MaskedCancellable.State]
) extends Cancellable:
    import MaskedCancellable.State

    def isCancelled: NonBlocking[Boolean] =
        ref.get match
            case State.Cancelled => true
            case _ => false

    inline def withCancellableThread[A](th: Thread)(inline block: NonBlocking[A]): NonBlocking[A] =
        setThread(th)
        try block
        finally mask()

    private def setThread(th: Thread): NonBlocking[Unit] =
        ref.updateAndGet:
            case State.Masked(true) => State.Cancelled
            case State.Masked(false) | State.Active(_) => State.Active(th)
            case State.Cancelled => State.Cancelled
        match
            case State.Cancelled =>
                if th != null then th.nn.interrupt()
            case State.Masked(_) =>
                throw IllegalStateException("Invalid state: Masked")
            case State.Active(_) =>
                ()

    private def mask(): NonBlocking[Unit] =
        ref.update:
            case State.Active(th) =>
                State.Masked(false)
            case other @ (State.Masked(_) | State.Cancelled) =>
                other

    def cancel(): NonBlocking[Unit] =
        ref.modify:
            case State.Active(th) => (State.Cancelled, th)
            case State.Masked(_) => (State.Masked(true), null)
            case other @ State.Cancelled => (other, null)
        match
            case null => ()
            case th: Thread => th.interrupt()
end MaskedCancellable

private object MaskedCancellable:
    def apply(): NonBlocking[MaskedCancellable] =
        new MaskedCancellable(Ref(State.Masked(false)))

    private enum State:
        case Active(th: Thread)
        case Masked(cancelled: Boolean)
        case Cancelled
