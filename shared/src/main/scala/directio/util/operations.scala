package directio
package util

inline def logAndIgnoreExceptions(
    f: Blocking[Unit]
)(using FailureReporter): Blocking[Unit] =
    try f
    catch
        case NonInterrupting(e) => summon[FailureReporter].reportFailure(e)
