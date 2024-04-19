package directio
package util

trait FailureReporter:
    def reportFailure(cause: Throwable): NonBlocking[Unit]

object FailureReporter:
    object default extends FailureReporter:
        given FailureReporter = this

        def reportFailure(cause: Throwable): NonBlocking[Unit] =
            cause.printStackTrace(System.err)
