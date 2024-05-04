package directio
package platform

private[platform] final class MaskedCancellable private (
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

private[platform] object MaskedCancellable:
    def apply(): NonBlocking[MaskedCancellable] =
        new MaskedCancellable(Ref(State.Masked(false)))

    private enum State:
        case Active(th: Thread)
        case Masked(cancelled: Boolean)
        case Cancelled
