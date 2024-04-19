package directio

import java.util.concurrent.CountDownLatch

type Callback[-A] = Outcome[A] => NonBlocking[Unit]

final class BlockingCallback[A] extends Callback[A]:
    private var result: Outcome[A] | Null = null
    private val latch = CountDownLatch(1)

    def apply(value: Outcome[A]): NonBlocking[Unit] =
        result = value
        latch.countDown()

    def await(): Blocking[Outcome[A]] =
        latch.await()
        result.asInstanceOf[Outcome[A]]
