package directio

import java.util.concurrent.CountDownLatch
// import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

class AsyncSuite extends munit.FunSuite:
    test("createCancellableFiber returns unstarted Fiber"):
        Blocking.run:
            val fiberStarted = new CountDownLatch(1)
            summon[Async].createCancellableFiber:
                fiberStarted.countDown()
            fiberStarted.awaitAndAssertFailure(50.millis)

    test("createCancellableFiber works"):
        Blocking.run:
            val gogo = new CountDownLatch(1)
            val fiberStarted = new CountDownLatch(1)
            val ref = Ref(10)

            val fiber = summon[Async].createCancellableFiber:
                fiberStarted.countDown()
                gogo.awaitAndAssert(5.seconds)
                ref.updateAndGet(_ + 10)

            fiber.start()
            fiberStarted.awaitAndAssert(5.seconds)
            assertEquals(ref.get, 10)
            gogo.countDown()
            assertEquals(fiber.awaitOutcome(), Outcome.Success(20))
            assertEquals(ref.get, 20)

    test("createCancellableFiber is cancellable"):
        Blocking.run:
            val wasStarted = new CountDownLatch(1)
            var wasCancelled = false
            val fiber = summon[Async].createCancellableFiber:
                wasStarted.countDown()
                try
                    Thread.sleep(10000)
                catch
                    case e: InterruptedException =>
                        wasCancelled = true
                        throw e

            fiber.start()
            wasStarted.awaitAndAssert(5.seconds)
            fiber.cancelAndAwaitOutcome() match
                case Outcome.Cancelled(_) => ()
                case other => fail(s"Expected Outcome.Cancelled, got $other")
            assert(wasCancelled)

    test("createUncancellableFiber returns unstarted Fiber"):
        Blocking.run:
            val fiberStarted = new CountDownLatch(1)
            summon[Async].createUncancellableFiber:
                fiberStarted.countDown()
            fiberStarted.awaitAndAssertFailure(50.millis)

    test("createUncancellableFiber works"):
        Blocking.run:
            val gogo = new CountDownLatch(1)
            val fiberStarted = new CountDownLatch(1)
            val ref = Ref(10)

            val fiber = summon[Async].createUncancellableFiber:
                fiberStarted.countDown()
                gogo.awaitAndAssert(5.seconds)
                ref.updateAndGet(_ + 10)

            fiber.start()
            fiberStarted.awaitAndAssert(5.seconds)
            assertEquals(ref.get, 10)
            gogo.countDown()
            assertEquals(fiber.awaitOutcome(), Outcome.Success(20))
            assertEquals(ref.get, 20)

    test("createUncancellableFiber is not cancellable"):
        Blocking.run:
            val wasStarted = new CountDownLatch(1)
            val fiberLatch = new CountDownLatch(1)
            var wasCancelled = false

            val fiber = summon[Async].createUncancellableFiber:
                wasStarted.countDown()
                try
                    fiberLatch.await()
                catch
                    case e: InterruptedException =>
                        wasCancelled = true
                        throw e

            fiber.start()
            wasStarted.awaitAndAssert(5.seconds)
            fiber.cancel()
            assert(fiber.outcome == null)
            fiberLatch.countDown()
            assert(fiber.awaitOutcome() == Outcome.Success(()))
            assert(!wasCancelled)

    test("uncancellable is uncancellable"):
        val step1FiberStarted = new CountDownLatch(1)
        val step2InFiber = new CountDownLatch(1)
        Blocking.run:
            val fiber = summon[Async].createUncancellableFiber:
                uncancellable: _ =>
                    step1FiberStarted.countDown()
                    step2InFiber.awaitAndAssert(5.seconds)
                    "done"

            fiber.start()
            step1FiberStarted.awaitAndAssert(5.seconds)
            fiber.cancel()
            step2InFiber.countDown()
            assertEquals(fiber.awaitOutcome(), Outcome.Success("done"))

    test("uncancellable can stack (1)"):
        val step1FiberStarted = new CountDownLatch(1)
        val step3AwaitInPoll = new CountDownLatch(1)

        Blocking.run:
            val fiber = summon[Async].createCancellableFiber:
                uncancellable: poll1 =>
                    poll1:
                        uncancellable: poll2 =>
                            poll2:
                                step1FiberStarted.countDown()
                                step3AwaitInPoll.awaitAndAssert(5.seconds)

            fiber.start()
            step1FiberStarted.awaitAndAssert(5.seconds)
            fiber.cancel()
            assert(fiber.awaitOutcome().isCancelled)

    test("uncancellable can stack (2)"):
        val step1FiberStarted = new CountDownLatch(1)
        val step3AwaitInPoll = new CountDownLatch(1)

        Blocking.run:
            val fiber = summon[Async].createCancellableFiber:
                uncancellable: poll1 =>
                    poll1:
                        uncancellable: _ =>
                            step1FiberStarted.countDown()
                            step3AwaitInPoll.awaitAndAssert(5.seconds)
                            "done"

            fiber.start()
            step1FiberStarted.awaitAndAssert(5.seconds)
            fiber.cancel()
            step3AwaitInPoll.countDown()
            assert(fiber.awaitOutcome() == Outcome.Success("done"))

    test("uncancellable works with poll, if cancellation happens before"):
        Blocking.run:
            val step1FiberStarted = new CountDownLatch(1)
            val step2AwaitSome = new CountDownLatch(1)
            val step3InPoll = new CountDownLatch(1)
            val hasResult = Ref(false)

            val fiber = summon[Async].createCancellableFiber:
                uncancellable: poll =>
                    step1FiberStarted.countDown()
                    step2AwaitSome.awaitAndAssert(5.seconds)
                    poll:
                        step3InPoll.awaitAndAssert(5.seconds)
                    hasResult.set(true)

            fiber.start()
            step1FiberStarted.awaitAndAssert(5.seconds)
            step2AwaitSome.countDown()
            fiber.cancel()
            assert(fiber.awaitOutcome().isCancelled)
            assert(hasResult.get == false)

    test("uncancellable works with poll, if cancellation happens in poll"):
        Blocking.run:
            val step1FiberStarted = new CountDownLatch(1)
            val step2InPoll = new CountDownLatch(1)
            val hasResult = Ref(false)

            val fiber = summon[Async].createCancellableFiber:
                uncancellable: poll =>
                    poll:
                        step1FiberStarted.countDown()
                        step2InPoll.awaitAndAssert(5.seconds)
                    hasResult.set(true)

            fiber.start()
            step1FiberStarted.awaitAndAssert(5.seconds)
            fiber.cancel()
            assert(fiber.awaitOutcome().isCancelled)
            assert(hasResult.get == false)
end AsyncSuite
