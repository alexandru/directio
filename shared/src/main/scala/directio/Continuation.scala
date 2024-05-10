package directio

trait Continuation[T]:
    def resume(outcome: Outcome[T]): NonBlocking[Unit]
    def get(): Blocking[T]
