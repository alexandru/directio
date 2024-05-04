package directio

enum Outcome[+A]:
    case Success(value: A) extends Outcome[A]
    case Failure(exception: Throwable) extends Outcome[Nothing]
    case Cancelled(exception: InterruptedException) extends Outcome[Nothing]

    def isCancelled: Boolean = this match
        case Cancelled(_) => true
        case _ => false

    def getOrThrow: A = this match
        case Success(a) => a
        case Failure(e) => throw e
        case Cancelled(e) => throw e

    inline def map[B](inline f: A => B): Outcome[B] = this match
        case Success(a) => Success(f(a))
        case fail @ Failure(_) => fail
        case cancel @ Cancelled(_) => cancel

    inline def void = map(_ => ())

object Outcome:
    def sequence[A](list: List[Outcome[A]]): Outcome[List[A]] =
        import Outcome.*
        list.foldRight(Outcome.Success(Nil): Outcome[List[A]]):
            case (Success(a), acc) =>
                acc match
                    case Success(rest) => Success(a :: rest)
                    case fail => fail
            case (fail @ Failure(e), acc) =>
                acc match
                    case Success(_) => fail
                    case Failure(eacc) =>
                        eacc.addSuppressed(e)
                        acc
                    case Cancelled(e2) =>
                        e.addSuppressed(e2)
                        fail
            case (cancel @ Cancelled(e), acc) =>
                acc match
                    // Not sure if this is correct
                    case Success(_) => cancel
                    case Failure(eacc) =>
                        eacc.addSuppressed(e)
                        acc
                    case Cancelled(eacc) =>
                        eacc.addSuppressed(e)
                        acc
    end sequence
end Outcome
