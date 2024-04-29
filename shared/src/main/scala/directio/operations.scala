package directio

import scala.util.control.NonFatal

private[directio] inline def logAndIgnoreExceptions(f: Blocking[Unit]): Blocking[Unit] =
    try f
    catch
        case NonFatal(e) => summon[Sync].reportFailure(e)

def scoped[T](block: Scope ?=> Blocking[T]): Blocking[T] =
    val ctx = Scope()
    try
        block(using ctx)
    finally
        ctx.shutdown()

def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A] =
    summon[Async].uncancellable(block)

def guaranteeCase[A](block: Blocking[A])(finalizer: Outcome[A] => Blocking[Unit]): Blocking[A] =
    summon[Async].guaranteeCase(block)(finalizer)

def fork[A](block: Blocking[A])(using Scope): Blocking[Fiber[A]] =
    val fiber = summon[Async].createCancellableFiber:
        try block
        finally summon[Scope].unregister(summon[Fiber[A]])

    summon[Scope].register(fiber) match
        case outcome: Outcome[A] => Fiber.completed(outcome)
        case () => fiber

    fiber.start()
    fiber
