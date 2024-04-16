package directio

trait FailureReporter:
    def reportFailure(cause: Throwable): NonBlocking[Unit]

object FailureReporter:
    object default extends FailureReporter:
        given FailureReporter = this

        def reportFailure(cause: Throwable): NonBlocking[Unit] =
            val th = Thread.currentThread()
            th.getUncaughtExceptionHandler().uncaughtException(th, cause)

object NonFatal:
    def apply(e: Exception): Boolean =
        e match
            case _: InterruptedException => false
            case _: Exception => true

    inline def unapply(e: Exception): Option[Exception] =
        if NonFatal(e) then Some(e) else None

inline def logAndIgnoreExceptions(f: Blocking[Unit])(using FailureReporter): Blocking[Unit] =
    try f
    catch
        case NonFatal(e) => summon[FailureReporter].reportFailure(e)
