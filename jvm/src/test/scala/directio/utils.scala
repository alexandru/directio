package directio

import munit.Location
import munit.Assertions.assert
import scala.concurrent.duration.FiniteDuration
import scala.quoted.*

extension (inline latch: java.util.concurrent.CountDownLatch)
    inline def awaitAndAssertFailure(fd: FiniteDuration)(implicit loc: Location): Unit =
        assert(
            !latch.await(fd.length, fd.unit),
            s"!${paramNameAsString(latch)}.await($fd)"
        )

    inline def awaitAndAssert(fd: FiniteDuration)(implicit loc: Location): Unit =
        assert(
            latch.await(fd.length, fd.unit),
            s"${paramNameAsString(latch)}.await($fd)"
        )

inline def paramNameAsString[T](inline param: T): String =
    ${ paramNameAsStringImpl('param) }

def paramNameAsStringImpl[T: Type](param: Expr[T])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val term = param.asTerm
    Expr(term.show)
