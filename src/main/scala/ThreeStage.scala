// ThreeStage.scala
package mrv

import chisel3._
import chisel3.util._

// -------------------- PipelineReg --------------------
class PipelineReg[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in     = Input(gen)
    val out    = Output(gen)
    val enable = Input(Bool())
    val flush  = Input(Bool())
  })

  val reg = RegInit(0.U.asTypeOf(gen))

  when(io.flush) {
    reg := 0.U.asTypeOf(gen)
  }.elsewhen(io.enable) {
    reg := io.in
  }

  io.out := reg
}

// -------------------- ThreeStageCPU --------------------
class ThreeStageCPU extends Module {
  val io = IO(new Bundle {
    val imem    = new MemoryPort
    val dmem    = new MemoryPort
    val signals = Output(new CpuSignals)
  })

  val test = IO(new Bundle {
    val pc = Output(UInt(32.W))
    val regs = new Bundle {
      val readAddr = Input(UInt(5.W))
      val readData = Output(UInt(32.W))
    }
  })

  // =====================
  //  IF + ID (Stage 1)
  // =====================
  val pc = RegInit(0.U(32.W))
  val pcPlus4 = pc + 4.U

  io.imem.addr := pc
  io.imem.memOp := MemOp.LW
  io.imem.writeData := 0.U

  val decoder = Module(new Decoder)
  decoder.io.inst := io.imem.readData

  val regFile = Module(new RegFile)
  regFile.io.rs1 := decoder.io.ctrl.rs1
  regFile.io.rs2 := decoder.io.ctrl.rs2
  regFile.io.rd := 0.U
  regFile.io.writeEnable := false.B
  regFile.io.writeData := 0.U

  regFile.test.reg := test.regs.readAddr
  test.regs.readData := regFile.test.regData

  val if_id = Module(new PipelineReg(new Bundle {
    val inst    = UInt(32.W)
    val pcPlus4 = UInt(32.W)
    val rs1     = UInt(5.W)
    val rs2     = UInt(5.W)
    val rd      = UInt(5.W)
    val ctrl    = new Ctrl
    val rs1Data = UInt(32.W)
    val rs2Data = UInt(32.W)
  }))

  if_id.io.in.inst := io.imem.readData
  if_id.io.in.pcPlus4 := pcPlus4
  if_id.io.in.rs1 := decoder.io.ctrl.rs1
  if_id.io.in.rs2 := decoder.io.ctrl.rs2
  if_id.io.in.rd := decoder.io.ctrl.rd
  if_id.io.in.ctrl := decoder.io.ctrl
  if_id.io.in.rs1Data := regFile.io.rs1Data
  if_id.io.in.rs2Data := regFile.io.rs2Data
  if_id.io.enable := true.B
  if_id.io.flush := false.B

  // =================
  //  EX (Stage 2)
  // =================
  val ex_alu = Module(new Alu)

  val forwardA = WireDefault(if_id.io.out.rs1Data)
  val forwardB = WireDefault(if_id.io.out.rs2Data)

  val ex_mem = Module(new PipelineReg(new Bundle {
    val ctrl    = new Ctrl
    val aluOut  = UInt(32.W)
    val rd      = UInt(5.W)
    val rs2Data = UInt(32.W)
  }))

  when(ex_mem.io.out.ctrl.regWrite && ex_mem.io.out.rd =/= 0.U) {
    when(ex_mem.io.out.rd === if_id.io.out.rs1) {
      forwardA := ex_mem.io.out.aluOut
    }
    when(ex_mem.io.out.rd === if_id.io.out.rs2) {
      forwardB := ex_mem.io.out.aluOut
    }
  }

  val mem_wb = Module(new PipelineReg(new Bundle {
    val ctrl   = new Ctrl
    val wbData = UInt(32.W)
    val rd     = UInt(5.W)
  }))

  when(mem_wb.io.out.ctrl.regWrite && mem_wb.io.out.rd =/= 0.U) {
    when(mem_wb.io.out.rd === if_id.io.out.rs1) {
      forwardA := mem_wb.io.out.wbData
    }
    when(mem_wb.io.out.rd === if_id.io.out.rs2) {
      forwardB := mem_wb.io.out.wbData
    }
  }

  ex_alu.io.op := if_id.io.out.ctrl.aluOp
  ex_alu.io.src1 := forwardA
  ex_alu.io.src2 := Mux(if_id.io.out.ctrl.useImm, if_id.io.out.ctrl.imm.asUInt, forwardB)


  ex_mem.io.in.aluOut := ex_alu.io.out

  ex_mem.io.in.ctrl := if_id.io.out.ctrl
  ex_mem.io.in.rd := if_id.io.out.rd
  ex_mem.io.in.rs2Data := forwardB // Fix forwarding for stores
  ex_mem.io.enable := true.B
  ex_mem.io.flush := false.B

  // =====================
  //  MEM + WB (Stage 3)
  // =====================
  io.dmem.addr := ex_mem.io.out.aluOut
  io.dmem.writeData := ex_mem.io.out.rs2Data
  io.dmem.memOp := ex_mem.io.out.ctrl.memOp

  // 確保對於非 LW 操作，wbData 使用 ALU 的輸出
  val wbData = Mux(
    ex_mem.io.out.ctrl.memOp === MemOp.LW,
    io.dmem.readData,
    ex_mem.io.out.aluOut
  )

  regFile.io.writeEnable := ex_mem.io.out.ctrl.regWrite
  regFile.io.writeData := wbData
  regFile.io.rd := ex_mem.io.out.rd

  mem_wb.io.in.ctrl := ex_mem.io.out.ctrl
  mem_wb.io.in.wbData := wbData
  mem_wb.io.in.rd := ex_mem.io.out.rd
  mem_wb.io.enable := true.B
  mem_wb.io.flush := false.B

  // ============================
  //  Branch & Hazard Detection
  // ============================
  val nextPC = Mux(ex_mem.io.out.ctrl.isBranch, ex_mem.io.out.aluOut, pcPlus4)

  val hazard = ex_mem.io.out.ctrl.regWrite &&
               ex_mem.io.out.rd =/= 0.U &&
               (ex_mem.io.out.rd === decoder.io.ctrl.rs1 ||
                ex_mem.io.out.rd === decoder.io.ctrl.rs2)

  val stall = WireDefault(false.B)
  when(hazard) {
    stall := true.B
  }

  when(!stall) {
    pc := nextPC
  }.otherwise {
    ex_mem.io.in.ctrl := 0.U.asTypeOf(ex_mem.io.in.ctrl)
    ex_mem.io.in.aluOut := 0.U
    ex_mem.io.in.rd := 0.U
    ex_mem.io.in.rs2Data := 0.U
  }

  if_id.io.enable := !stall

  io.signals.halted := ex_mem.io.out.ctrl.exception
  test.pc := pc
}

// -------------------- ThreeStageSim (測試用) --------------------
class ThreeStageSim(init: List[Int] = List()) extends Module {
  val signals = IO(Output(new CpuSignals))

  // 實例化指令記憶體
  val imemSim = Module(new MemorySim(init))

  // 實例化數據記憶體
  val dmemSim = Module(new MemorySim())

  val core = Module(new ThreeStageCPU)

  val test = IO(new Bundle {
    val loaded = Output(Bool())
    val pc     = Output(UInt(32.W))
    val regs = new Bundle {
      val readAddr = Input(UInt(5.W))  // <-- Change to 5 bits, corresponding to 0~31
      val readData = Output(UInt(32.W))
    }
  })

  // 連接指令記憶體
  imemSim.io.mem <> core.io.imem

  // 連接數據記憶體
  dmemSim.io.mem <> core.io.dmem

  // 連接測試輸出
  test.loaded := imemSim.io.loaded && dmemSim.io.loaded
  test.pc     := core.test.pc
  test.regs <> core.test.regs

  signals <> core.io.signals

  // 當記憶體尚未載入完成時，拉高 reset
  core.reset := !(imemSim.io.loaded && dmemSim.io.loaded)
}
