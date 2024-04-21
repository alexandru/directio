package directio

private enum ContextState:
    case Running(children: List[Fiber[?]], join: Deferred[Unit])
    case Stopping(children: List[Fiber[?]], join: Deferred[Unit])
    case Stopped

final class Scope(state: Ref[ContextState]):
    import ContextState.*

    def register[A](token: Fiber[A]): Blocking[Unit | Outcome[A]] =
        state.modify:
            case Running(children, join) =>
                (Running(token :: children, join), null)
            case Stopping(children, join) =>
                (Stopping(token :: children, join), null)
            case Stopped =>
                (Stopped, token)
        match
            case null => ()
            case token: Fiber[A @unchecked] =>
                token.cancelAndJoin()

    def unregister(id: FiberId): NonBlocking[Unit] =
        state.update:
            case Running(children, join) =>
                Running(children.filterNot(_.id == id), join)
            case Stopping(children, join) =>
                Stopping(children.filterNot(_.id == id), join)
            case Stopped =>
                Stopped

    def unregister(token: Fiber[?]): NonBlocking[Unit] =
        unregister(token.id)

    private def stopAll(outcomes: List[Outcome[?]]): Blocking[Unit] =
        state.modify:
            case Running(children, join) =>
                (Stopping(children, join), true)
            case Stopping(list, join) =>
                list match
                    case first :: rest => (Stopping(rest, join), first)
                    case Nil => (Stopped, join)
            case Stopped =>
                (Stopped, false)
        match
            case true => stopAll(outcomes) // retry
            case false => () // done
            case join: Deferred[Unit] => // done + signal
                join.complete(Outcome.sequence(outcomes).void)
            case fiber: Fiber[?] => // done + error
                fiber.cancelAndJoin()
                stopAll(outcomes) // retry

    private[directio] def shutdown(): Blocking[Unit] =
        state.modify:
            case Running(children, join) =>
                (Stopping(children, join), true)
            case current @ Stopping(_, join) =>
                (current, join)
            case Stopped =>
                (Stopped, false)
        match
            case true => stopAll(Nil)
            case false => ()
            case join: Deferred[Unit] =>
                join.awaitComplete() match
                    case Outcome.Success(_) | Outcome.Cancelled(_) => ()
                    case Outcome.Failure(e) => throw e
end Scope

object Scope:
    def apply(): NonBlocking[Scope] =
        new Scope(Ref(ContextState.Running(Nil, Deferred())))
