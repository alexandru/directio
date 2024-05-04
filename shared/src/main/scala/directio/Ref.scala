package directio

import java.util.concurrent.atomic.AtomicReference

/** A mutable reference that can be updated atomically and safely.
  *
  * Wraps `java.util.concurrent.atomic.AtomicReference` in a safer
  * API.
  */
opaque type Ref[A] = AtomicReference[A]

object Ref:
    def apply[A](initialValue: A): NonBlocking[Ref[A]] =
        AtomicReference(initialValue)

    extension [State](ref: Ref[State])
        def get: NonBlocking[State] =
            ref.get

        def set(value: State): NonBlocking[Unit] =
            ref.set(value)

        def compareAndSet(expect: State, update: State): NonBlocking[Boolean] =
            ref.compareAndSet(expect, update)

        def getAndSet(newValue: State): NonBlocking[State] =
            ref.getAndSet(newValue)

        inline def updateAndGet(inline f: State => State): NonBlocking[State] =
            modify: state =>
                val newState = f(state)
                (newState, newState)

        inline def getAndUpdate(inline f: State => State): NonBlocking[State] =
            modify: state =>
                val newState = f(state)
                (newState, state)

        inline def update(inline f: State => State): NonBlocking[Unit] =
            modify: state =>
                val newState = f(state)
                (newState, ())

        inline def modify[Result](inline f: State => (State, Result)): NonBlocking[Result] =
            var hasRet = false
            var ret: Result | Null = null
            while !hasRet do
                val current = ref.get
                val (next, result) = f(current)
                hasRet = ref.compareAndSet(current, next)
                if hasRet then
                    ret = result
            ret.asInstanceOf[Result]
    end extension
end Ref
