package directio

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

class AsyncSuite extends munit.FunSuite:
    test("forkUnsafe works"):
        Blocking.run:
            val gogo = new CountDownLatch(1)
            val fiberStarted = new CountDownLatch(1)
            val ref = Ref(10)

            val fiber = summon[Async].forkUnsafeInterruptible: _ =>
                fiberStarted.countDown()
                gogo.await()
                ref.updateAndGet(_ + 10)

            fiberStarted.await()
            assertEquals(ref.get, 10)
            gogo.countDown()
            assertEquals(fiber.join(), Outcome.Success(20))
            assertEquals(ref.get, 20)

    test("forkUnsafe is cancellable"):
        Blocking.run:
            val wasStarted = new CountDownLatch(1)
            var wasCancelled = false
            val fiber = summon[Async].forkUnsafeInterruptible: _ =>
                wasStarted.countDown()
                try
                    Thread.sleep(10000)
                catch
                    case e: InterruptedException =>
                        wasCancelled = true
                        throw e

            wasStarted.await()
            fiber.cancelAndJoin() match
                case Outcome.Cancelled(_) => ()
                case other => fail(s"Expected Outcome.Cancelled, got $other")
            assert(wasCancelled)

    test("uncancellable works with poll, if cancellation happens before"):
        val step1FiberStarted = new CountDownLatch(1)
        val step2AwaitSome = new CountDownLatch(1)
        val step3InPoll = new CountDownLatch(1)
        val wasCancelled = new CountDownLatch(1)
        Blocking.run:
            val fiber = summon[Async].forkUnsafeInterruptible: _ =>
                uncancellable: poll =>
                    try
                        step1FiberStarted.countDown()
                        step2AwaitSome.await()
                        poll:
                            step3InPoll.await(5000, TimeUnit.MILLISECONDS)
                    catch
                        case _: InterruptedException =>
                            wasCancelled.countDown()

            step1FiberStarted.await()
            step2AwaitSome.countDown()
            assert(!wasCancelled.await(50, TimeUnit.MILLISECONDS))
            fiber.cancel()
            assert(wasCancelled.await(50, TimeUnit.MILLISECONDS))

    test("uncancellable works with poll, if cancellation happens in poll"):
        val step1FiberStarted = new CountDownLatch(1)
        val step2InPoll = new CountDownLatch(1)
        val wasCancelled = new CountDownLatch(1)
        Blocking.run:
            val fiber = summon[Async].forkUnsafeInterruptible: _ =>
                uncancellable: poll =>
                    try
                        poll:
                            step1FiberStarted.countDown()
                            step2InPoll.await(5000, TimeUnit.MILLISECONDS)
                    catch
                        case _: InterruptedException =>
                            wasCancelled.countDown()

            step1FiberStarted.await()
            fiber.cancel()
            assert(wasCancelled.await(50, TimeUnit.MILLISECONDS))
end AsyncSuite
