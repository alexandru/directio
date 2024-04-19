// package directio

// class ScopeForkSuite extends munit.FunSuite:
//     import FailureReporter.default.given

//     test("fork")(Blocking.run:
//         var wasWritten = false
//         val th = Thread.currentThread()
//         scoped:
//             fork:
//                 assert(Thread.currentThread() != th)
//                 wasWritten = true
//         assertEquals(wasWritten, true)
//     )
