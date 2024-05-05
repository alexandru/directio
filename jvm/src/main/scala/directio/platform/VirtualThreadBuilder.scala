package directio
package platform

private[platform] opaque type VirtualThreadBuilder <: Thread.Builder =
    Thread.Builder

private[platform] object VirtualThreadBuilder:
    def apply(): NonBlocking[VirtualThreadBuilder] =
        Thread.ofVirtual()
            .name("directio-fiber")
            .uncaughtExceptionHandler((_, e) =>
                NonBlocking.run(summon[Sync].reportFailure(e))
            )
