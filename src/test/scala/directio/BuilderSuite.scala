package directio

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class BuilderSuite extends munit.FunSuite:
    test("NonBlocking.run"):
        val ref = VarRef(0)
        NonBlocking.run:
            ref.update(_ + 1)
            ref.update(_ + 1)
        assertEquals(ref.value, 2)

    test("Blocking.run"):
        val ref = VarRef(0)
        Blocking.run:
            ref.update(_ + 1)
            ref.update(_ + 1)
        assertEquals(ref.value, 2)

    class VarRef[A](var value: A):
        def update(f: A => A): NonBlocking[A] =
            value = f(value)
            value
