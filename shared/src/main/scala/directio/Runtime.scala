package directio

import directio.util.*

type Poll[A] = NonBlocking[A] => NonBlocking[A]

trait Runtime extends FailureReporter:
    def start[A](block: Blocking[A]): NonBlocking[Fiber[A]]

    def uncancellable[A](block: Poll[A] => Blocking[A]): Blocking[A]
end Runtime

private[directio] trait RuntimeCompanion:
    def global: Runtime

object Runtime extends RuntimeCompanion
    with directio.platform.RuntimeCompanionPerPlatform
