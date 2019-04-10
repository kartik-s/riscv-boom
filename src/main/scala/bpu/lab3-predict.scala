package boom

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.{Parameters, Field}

/*
 * A table of (1 << width) two-bit predictors.
 */
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

  // update
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

  // predict
  io.prediction := predictors(io.index)(1)
}

/*
 * Common bundle for predictor IO. The update sub-bundle is used to pass
 * information needed for updating the predictor.
 */
class PredictorIO(addrWidth: Int, historyLen: Int) extends Bundle {
  val addr = Input(UInt(addrWidth.W))
  val history = Input(UInt(historyLen.W))
  val prediction = Output(Bool())

  val update = new Bundle {
    val en = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val history = Input(UInt(historyLen.W))
    val taken = Input(Bool())
    val prediction = Input(Bool())
  }
}

/*
 * Two-level local predictor. Top level contains local histories indexed by
 * branch address, second level contains two-bit predictors indexed by
 * local history.
 */
class LocalPredictor(width: Int, historyLen: Int) extends Module {
  val io = IO(new PredictorIO(
    addrWidth = width,
    historyLen = historyLen
  ))

  // top-level
  val localHistory = RegInit(VecInit(
    Seq.fill(1 << width)(0.U(historyLen.W))
  ))
  // two-bit predictors
  val predictors = Module(new TBPTable(width = historyLen))

  // update predictor
  val updateAddr = io.update.addr(width - 1, 0)

  predictors.io.update.en := io.update.en
  predictors.io.update.index := localHistory(updateAddr)
  predictors.io.update.taken := io.update.taken

  // update local history
  localHistory(updateAddr) := (localHistory(updateAddr) << 1) ^ io.update.taken

  // predict
  predictors.io.index := localHistory(io.addr(width - 1, 0))
  io.prediction := predictors.io.prediction
}

/*
 * A predictor consisting of a table of two-bit predictors indexed by some
 * combination of branch address and global history specified by a hash
 * function.
 */
class HybridPredictor(width: Int, hash: (UInt, UInt) => UInt) extends Module {
  require (width >= 0)

  val io = IO(new PredictorIO(
    addrWidth = width,
    historyLen = width
  ))

  val table = Module(new TBPTable(width))

  // update
  table.io.update.en := io.update.en
  table.io.update.taken := io.update.taken
  table.io.update.index := hash(io.update.addr, io.update.history)

  // predict
  table.io.index := hash(io.addr, io.history)
  io.prediction := table.io.prediction
}

/*
 * A tournament predictor consisting of a local predictor, a global predictor,
 * and a selector to choose between the two predictors.
 */
class TourneyPredictor(historyLen: Int) extends Module {
  val io = IO(new PredictorIO(
    addrWidth = historyLen,
    historyLen = historyLen
  ))

  val selector = Module(new HybridPredictor(
    width = historyLen,
    hash = (addr, _) => addr(historyLen - 1, 0)
  ))
  val globalPredictor = Module(new HybridPredictor(
    width = historyLen,
    hash = (addr, hist) => addr ^ hist
  ))
  val localPredictor = Module(new LocalPredictor(
    width = historyLen,
    historyLen = historyLen
  ))

  // the last selection direction
  val lastSelect = RegNext(selector.io.prediction)

  // update selector
  selector.io.update <> io.update
  selector.io.update.taken := Mux(
    io.update.taken === io.update.prediction,
    lastSelect,
    !lastSelect
  )

  // update predictor
  localPredictor.io.update <> io.update
  globalPredictor.io.update <> io.update

  // predict
  selector.io.addr := io.addr
  selector.io.history := io.history

  localPredictor.io.addr := io.addr
  localPredictor.io.history := io.history

  globalPredictor.io.addr := io.addr
  globalPredictor.io.history := io.history

  io.prediction := Mux(selector.io.prediction,
    globalPredictor.io.prediction,
    localPredictor.io.prediction)
}

/*
 * Bundle for organizing info passed back to the branch predictor through
 * the last commit.
 */
class CommitInfo(addrWidth: Int, historyLen: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val history = UInt(historyLen.W)
  val prediction = Bool()

  override def cloneType = new CommitInfo(addrWidth, historyLen).asInstanceOf[this.type]
}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 10, info_size: Int = 21)

case object Lab3Key extends Field[Lab3Parameters]

/*
 * io.resp.ready -> is the processor ready for a prediction?
 * io.resp.valid -> tells the processor if there's a prediction
 * io.resp.bits.takens -> the actual prediction
 * io.resp.bits.info -> stash info, get it back on the next update
 *
 * io.req_pc -> the current branch's address
 *
 * this.commit -> how the last prediction went
 * this.commit.valid -> is the update valid?
 * this.commit.bits.info.info -> what was stashed last time
 * this.commit.bits.ctrl.taken(0) -> was the last branch taken?
 *
 * this.ghistory -> current global history
 *
 * this.disable_bpd -> disable prediction?
 */
class Lab3BrPredictor(
  fetch_width: Int,
  history_length: Int)(implicit p: Parameters)
extends BrPredictor(fetch_width, history_length)(p)
{
  // Create the actual predictor
  val predictor = Module(new TourneyPredictor(
    historyLen = history_length
  ))

  // Update the predictor with the previous commit result
  predictor.io.update.en := this.commit.valid
  predictor.io.update.taken := this.commit.bits.ctrl.taken(0)

  val commitInfo = (new CommitInfo(history_length, history_length)).fromBits(this.commit.bits.info.info)

  predictor.io.update.addr := commitInfo.addr
  predictor.io.update.history := commitInfo.history
  predictor.io.update.prediction := commitInfo.prediction

  predictor.io.addr := io.req_pc
  predictor.io.history := this.ghistory

  // Output the prediction for the current branch
  io.resp.valid := io.resp.ready && !this.disable_bpd
  io.resp.bits.takens := predictor.io.prediction

  // Stash the current branch address and global history for the next update
  val info = Wire(new CommitInfo(history_length, history_length))

  info.addr := io.req_pc
  info.history := this.ghistory
  info.prediction := predictor.io.prediction
  io.resp.bits.info := info.asUInt()
}

