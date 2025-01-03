package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object AluOp extends ChiselEnum {
  val NONE = Value
  val ADD  = Value
  val AND  = Value
  val EQ   = Value
  val GE   = Value
  val GEU  = Value
  val LT   = Value
  val LTU  = Value
  val NE   = Value
  val NEQ  = Value
  val OR   = Value
  val SLL  = Value
  val SLT  = Value
  val SLTU = Value
  val SRA  = Value
  val SRL  = Value
  val SUB  = Value
  val XOR  = Value

  // M extension
  val MUL    = Value
  val MULH   = Value
  val MULHSU = Value
  val MULHU  = Value
  val DIV    = Value
  val DIVU   = Value
  val REM    = Value
  val REMU   = Value
}

class Alu extends Module {
  val io = IO(new Bundle {
    val op   = Input(AluOp())
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val out  = Output(UInt(32.W))
  })

  io.out := 0.U

  val src1S = io.src1.asSInt
  val src2S = io.src2.asSInt

  switch(io.op) {
    is(AluOp.ADD) {
      io.out := io.src1 + io.src2
    }
    is(AluOp.SUB) {
      io.out := io.src1 - io.src2
    }
    is(AluOp.AND) {
      io.out := io.src1 & io.src2
    }
    is(AluOp.EQ) {
      io.out := io.src1 === io.src2
    }
    is(AluOp.NE) {
      io.out := io.src1 =/= io.src2
    }
    is(AluOp.LT) {
      io.out := io.src1.asSInt < io.src2.asSInt
    }
    is(AluOp.LTU) {
      io.out := io.src1 < io.src2
    }
    is(AluOp.GE) {
      io.out := io.src1.asSInt >= io.src2.asSInt
    }
    is(AluOp.GEU) {
      io.out := io.src1 >= io.src2
    }
    is(AluOp.SRA) {
      io.out := (io.src1.asSInt >> io.src2(4, 0)).asUInt
    }
    is(AluOp.SRL) {
      io.out := io.src1 >> io.src2(4, 0)
    }
    is(AluOp.SLL) {
      io.out := io.src1 << io.src2(4, 0)
    }
    is(AluOp.OR) {
      io.out := io.src1 | io.src2
    }
    is(AluOp.XOR) {
      io.out := io.src1 ^ io.src2
    }

    // ---------------------- M extension ---------------------- //
    is(AluOp.MUL) {
      io.out := (io.src1 * io.src2)(31, 0)
    }
    is(AluOp.MULH) {
      // High 32 bits of 4-bit product, signed multiplication
      val product = (src1S * src2S).asSInt
      io.out := product(63, 32).asUInt
    }
    is(AluOp.MULHSU) {
      // src1 has a number, src2 has no number
      val product = (src1S * io.src2).asSInt
      io.out := product(63, 32).asUInt
    }
    is(AluOp.MULHU) {
      // All without numbers
      val product = (io.src1 * io.src2)
      io.out := product(63, 32)
    }
    is(AluOp.DIV) {
      when(src2S === 0.S) {
        // Divide by 0, the result is undefined
        io.out := "hFFFFFFFF".U
      }.otherwise {
        io.out := (src1S / src2S).asUInt
      }
    }
    is(AluOp.DIVU) {
      when(io.src2 === 0.U) {
        io.out := "hFFFFFFFF".U
      }.otherwise {
        io.out := io.src1 / io.src2
      }
    }
    is(AluOp.REM) {
      when(src2S === 0.S) {
        io.out := src1S.asUInt
      }.otherwise {
        io.out := (src1S % src2S).asUInt
      }
    }
    is(AluOp.REMU) {
      when(io.src2 === 0.U) {
        io.out := io.src1
      }.otherwise {
        io.out := io.src1 % io.src2
      }
    }
    // ---------------------- M extension ---------------------- //
  }
}
