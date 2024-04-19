package directio

import java.util.concurrent.CountDownLatch

class RuntimeSuite extends munit.FunSuite:
    test("start works"):
        Blocking.run:
            val gogo = new CountDownLatch(1)
            val fiberStarted = new CountDownLatch(1)
            val ref = Ref(10)

            val fiber = Runtime.global.start:
                fiberStarted.countDown()
                gogo.await()
                ref.updateAndGet(_ + 10)

            fiberStarted.await()
            assertEquals(ref.get, 10)
            gogo.countDown()
            assertEquals(fiber.join(), Outcome.Success(20))
            assertEquals(ref.get, 20)

    test("fiber is cancellable"):
        Blocking.run:
            val wasStarted = new CountDownLatch(1)
            var wasCancelled = false
            val fiber = Runtime.global.start:
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
