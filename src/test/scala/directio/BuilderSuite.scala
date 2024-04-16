package directio

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class BuilderSuite extends munit.FunSuite:
    test("NonBlocking.run"):
        val ref = Ref(0)
        NonBlocking.run:
            ref.update(_ + 1)
            ref.update(_ + 1)
        assertEquals(ref.value, 2)

    test("Blocking.run"):
        val ref = Ref(0)
        Blocking.runUnsafeExceptions:
            ref.update(_ + 1)
            ref.update(_ + 1)
        assertEquals(ref.value, 2)

    class Ref[A](var value: A):
        def update(f: A => A): NonBlocking[A] =
            value = f(value)
            value
