package directio

def scoped[T](f: Scope ?=> Blocking[T])(using FailureReporter): Blocking[T] =
    val ctx = Scope()
    try
        f(using ctx)
    finally
        ctx.shutdown()

def fork[T](body: Blocking[T])(using Scope): Blocking[Fiber[T]] =
    given FailureReporter = summon[Scope].failureReporter
    val deferred = Deferred[T]()
    val th = Thread.ofVirtual().unstarted(() => deferred.completeWith(body))

    val fiber = new Fiber[T]:
        def join(): Blocking[Outcome[T]] =
            deferred.awaitComplete()
        def cancel(): Blocking[Unit] =
            th.interrupt()
            th.join()

    summon[Scope].register(fiber)
    th.start()
    fiber

def uncancellable[T](block: Blocking[T])(using Scope): Blocking[T] =
    given FailureReporter = summon[Scope].failureReporter
    val deferred = Deferred[T]()
    val th = Thread.ofVirtual().start(() =>
        Blocking.run:
            deferred.completeWith(block)
    )
    val fiber = new Fiber[T]:
        def join(): Blocking[Outcome[T]] =
            deferred.awaitComplete()
        def cancel(): NonBlocking[Unit] =
            th.join()
    summon[Scope].register(fiber)
    th.start()
    var interrupted = false
    while th.isAlive() do
        interrupted = interrupted || Thread.interrupted()
        th.join()
    if interrupted then throw InterruptedException()
    deferred.awaitComplete().getOrThrow

enum CancelToken:
    case Uncancellable
    case CancellableRef(token: Cancellable)

def async[A](f: Callback[A] => Blocking[CancelToken])(using Scope): Blocking[A] =
    given FailureReporter = summon[Scope].failureReporter
    val deferred = Deferred[A]()
    val cancelRef = MultiAssignCancellable()

    val fiber = fork:
        f(deferred.complete) match
            case CancelToken.Uncancellable => ()
            case CancelToken.CancellableRef(token) =>
                cancelRef.set(token)

    try
        fiber.join()
        deferred.awaitComplete().getOrThrow
    catch
        case e: InterruptedException =>
            fiber.cancel()
            cancelRef.cancel()
            throw e

    fiber.join()
    deferred.awaitComplete().getOrThrow
