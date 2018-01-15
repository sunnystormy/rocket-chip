// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case class SystemBusParams(
  beatBytes: Int,
  blockBytes: Int,
  slaveBuffering: BufferParams = BufferParams.default
) extends TLBusParams

case object SystemBusKey extends Field[SystemBusParams]

class SystemBus(params: SystemBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "SystemBus") {

  private val master_splitter = LazyModule(new TLSplitter)
  private val master_splitter = LazyModule(new TLSplitter)
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edges.in.head

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = master_splitter.node

  def toSplitSlaves: TLOutwardNode = outwardSplitNode
      TLBuffer.from(buffers)(
        TLSplitter.from(TLArbiter.roundRobin)(
  master_splitter.node :=* port_fixer.node

  def toPeripheryBus(addBuffers: Int = 0)(gen: TLAdaptingTo) {
    to("PeripheryBus")(
      TLBuffer.to(addBuffers)(
        TLFixer.to(TLFIFOFixer.all)(
          TLWidthWidget.to(beatBytes)(
            TLBuffer.to(BufferParams.default)(gen)))))
  }

  val toMemoryBus(gen: TLAdaptingTo) {
    to("MemoryBus")(gen)
  }

  val toSlave(name: Option[String] = None)(gen: TLAdaptingTo) {
    to(s"Slave${name.getOrElse("")}")(
      TLBuffer.to(BufferParams.default)(gen))
  }

  def fromCoherentChip(gen: TLAdaptingFrom) {
    from("CoherentChip")(gen)
  }

  def fromFrontBus(gen: TLAdaptingFrom) {
    from("FrontBus"){ TLSplitter.from(gen) }
  }

  def fromTile(name: Option[String])(gen: TLAdaptingFrom) {
    from(s"Tile${name.getOrElse("")}") { TLSplitter.from(gen) }
  }

  def fromPort(buffers: Int = 0, name: Option[String] = None)(gen: TLAdapatingFrom) {
    from(s"Port${name.getOrElse("")}") {
        TLSplitter.from(TLArbiter.roundRobin)(
          TLFixer.from((TLFIFOFixer.all)(
            TLBuffer.from(buffers)(gen))))
    }
  }
}

/** Provides buses that serve as attachment points,
  * for use in traits that connect individual devices or external ports.
  */
trait HasSystemBus extends HasInterruptBus {
  private val sbusParams = p(SystemBusKey)
  val sbusBeatBytes = sbusParams.beatBytes

  val sbus = LazyModule(new SystemBus(sbusParams))

  def sharedMemoryTLEdge: TLEdge = sbus.busView
}
