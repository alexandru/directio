package directio.util

object NonInterrupting:
    def apply(e: Exception): Boolean =
        e match
            case _: InterruptedException => false
            case _: Exception => true

    inline def unapply(e: Exception): Option[Exception] =
        if NonInterrupting(e) then Some(e) else None
