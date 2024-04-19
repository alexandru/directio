// package directio

// private enum ContextState:
//     case Running(children: List[Fiber[?]], join: Deferred[Unit])
//     case Stopping(children: List[Fiber[?]], join: Deferred[Unit])
//     case Stopped

// final class Scope(state: Ref[ContextState])(using FailureReporter):
//     import ContextState.*

//     def failureReporter = summon[FailureReporter]

//     def register(fiber: Fiber[?]): Blocking[Unit | Outcome[?]] =
//         state.modify:
//             case Running(children, join) =>
//                 (Running(fiber :: children, join), null)
//             case Stopping(children, join) =>
//                 (Stopping(fiber :: children, join), null)
//             case Stopped =>
//                 (Stopped, fiber)
//         match
//             case null => ()
//             case fiber => fiber.cancelAndJoin()

//     private def stopAll(outcomes: List[Outcome[?]]): Blocking[Unit] =
//         state.modify:
//             case Running(children, join) =>
//                 (Stopping(children, join), true)
//             case Stopping(list, join) =>
//                 list match
//                     case first :: rest => (Stopping(rest, join), first)
//                     case Nil => (Stopped, join)
//             case Stopped =>
//                 (Stopped, false)
//         match
//             case true => stopAll(outcomes) // retry
//             case false => () // done
//             case join: Deferred[Unit] => // done + signal
//                 join.complete(Outcome.sequence(outcomes).void)
//             case fiber: Fiber[?] => // done + error
//                 fiber.cancelAndJoin()
//                 stopAll(outcomes) // retry

//     def shutdown(): Blocking[Unit] =
//         state.modify:
//             case Running(children, join) =>
//                 (Stopping(children, join), true)
//             case current @ Stopping(_, join) =>
//                 (current, join)
//             case Stopped =>
//                 (Stopped, false)
//         match
//             case true => stopAll(Nil)
//             case false => ()
//             case join: Deferred[Unit] =>
//                 join.awaitComplete() match
//                     case Outcome.Success(_) | Outcome.Cancelled(_) => ()
//                     case Outcome.Failure(e) => throw e
// end Scope

// object Scope:
//     def apply()(using FailureReporter): NonBlocking[Scope] =
//         new Scope(Ref(ContextState.Running(Nil, Deferred())))
