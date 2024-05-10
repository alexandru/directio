package directio
package platform

private[platform] def cont[A](
    block: Continuation[A] => Blocking[A]
): Blocking[A] =
    val deferred = Deferred[A]()
    val cont = new Continuation[A]:
        def resume(outcome: Outcome[A]): NonBlocking[Unit] =
            deferred.complete(outcome)

        def get(): Blocking[A] =
            deferred.awaitComplete().getOrThrow

    block(cont)
