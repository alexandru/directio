package directio

trait Cancellable:
    def cancel(): NonBlocking[Unit]

object Cancellable:
    inline def apply(f: NonBlocking[Unit]): Cancellable =
        new Cancellable:
            def cancel() = f

    val empty: Cancellable =
        new Cancellable:
            def cancel() = ()
