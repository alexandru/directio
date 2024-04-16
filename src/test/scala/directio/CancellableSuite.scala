package directio

class CancellableSuite extends munit.FunSuite:
    test("idempotency"):
        var counter = 0
        Blocking.run:
            val cancellable = Cancellable.idempotent(counter += 1)
            assertEquals(counter, 0)
            cancellable.cancel()
            cancellable.cancel()
        assertEquals(counter, 1)
