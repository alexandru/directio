package directio
package util

import scala.util.control.NonFatal

inline def logAndIgnoreExceptions(
    f: Blocking[Unit]
)(using FailureReporter): Blocking[Unit] =
    try f
    catch
        case NonFatal(e) => summon[FailureReporter].reportFailure(e)
