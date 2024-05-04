package directio

trait Continuation[T]:
    def resume(value: Either[Throwable, T]): NonBlocking[Unit]
    def get(): Blocking[T]
