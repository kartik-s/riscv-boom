package boom

// import Chisel._
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}

class TBPTable(width: Int) extends Module {
  require(width >= 0)

  val io = IO(new Bundle {
    val index = Input(UInt(width.W))
    val prediction = Output(Bool())

    val update = new Bundle {
      val en = Input(Bool())
      val index = Input(UInt(width.W))
      val taken = Input(Bool())
    }
  })

  val predictors = RegInit(VecInit(Seq.fill(1 << width)(0.U(2.W))))

  val updated = predictors(io.update.index)

  when (io.update.en) {
    when (updated === 0.U) {
      updated := Mux(io.update.taken, 1.U, 0.U)
    } .elsewhen (updated === 3.U) {
      updated := Mux(io.update.taken, 3.U, 2.U)
    } .otherwise {
      updated := Mux(io.update.taken, 3.U, 0.U)
    }
  }

  io.prediction := predictors(io.index)(1)
}

class HybridPredictor(width: Int, hash: (UInt, UInt) => UInt) extends Module {
  require (width >= 0)

  val io = IO(new Bundle {
    val addr = Input(UInt(width.W))
    val history = Input(UInt(width.W))
    val prediction = Output(Bool())

    val update = new Bundle {
      val en = Input(Bool())
      val addr = Input(UInt(width.W))
      val history = Input(UInt(width.W))
      val taken = Input(Bool())
    }
  })

  val table = Module(new TBPTable(width))

  table.io.update.en := io.update.en
  table.io.update.taken := io.update.taken
  table.io.update.index := hash(io.update.addr, io.update.history)

  table.io.index := hash(io.addr, io.history)
  io.prediction := table.io.prediction
}

class InfoBundle(addrWidth: Int, historyLen: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val history = UInt(historyLen.W)

  override def cloneType = new InfoBundle(addrWidth, historyLen).asInstanceOf[this.type]
}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 10, info_size: Int = 20)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
  fetch_width: Int,
  history_length: Int)(implicit p: Parameters)
extends BrPredictor(fetch_width, history_length)(p)
{
  // io.resp.ready -> is the processor ready for a prediction?
  // io.resp.valid -> tells the processor if there's a prediction
  // io.resp.bits.takens -> the actual prediction
  // io.resp.bits.info -> stash info, get it back on the next update
  //
  // io.req_pc -> the current branch's address
  //
  // this.commit -> how the last prediction went
  // this.commit.valid -> is the update valid?
  // this.commit.bits.info.info -> what was stashed last time
  // this.commit.bits.ctrl.taken(0) -> was the last branch taken?
  //
  // this.ghistory -> current global history
  //
  // this.disable_bpd -> disable prediction?

  val predictor = Module(new HybridPredictor(10, {_ ^ _}))

  predictor.io.update.en := this.commit.valid
  predictor.io.update.taken := this.commit.bits.ctrl.taken(0)

  val commitInfo = (new InfoBundle(10, 10)).fromBits(this.commit.bits.info.info)

  predictor.io.update.addr := commitInfo.addr
  predictor.io.update.history := commitInfo.history

  predictor.io.addr := io.req_pc
  predictor.io.history := this.ghistory

  io.resp.valid := io.resp.ready && !this.disable_bpd
  io.resp.bits.takens := predictor.io.prediction

  val info = Wire(new InfoBundle(10, 10))

  info.addr := io.req_pc
  info.history := this.ghistory
  io.resp.bits.info := info.asUInt()
}
