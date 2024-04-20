package directio
package platform

private[directio]
inline def keepTryingDespiteInterruption[A](block: Blocking[A]): Blocking[A] =
    var continue = true
    var ret: A | Null = null
    var wasInterrupted: Throwable | Null = null

    while continue do
        try
            ret = block
            continue = false
        catch
            case e: InterruptedException =>
                Thread.interrupted()
                if wasInterrupted == null then
                    wasInterrupted = e
                else
                    wasInterrupted.addSuppressed(e)

    if wasInterrupted != null then
        throw wasInterrupted
    ret.asInstanceOf[A]
