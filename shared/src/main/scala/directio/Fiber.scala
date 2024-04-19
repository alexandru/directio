package directio

trait Fiber[+A] extends Cancellable:
    def join(): Blocking[Outcome[A]]

    def cancelAndJoin(): Blocking[Outcome[A]] =
        cancel()
        join()
