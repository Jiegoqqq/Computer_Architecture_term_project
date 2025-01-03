package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import com.carlosedp.riscvassembler.RISCVAssembler

import chisel3._
import chiseltest._

class DecoderSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decoder"
  // Remove or comment out the original "not decode MUL"
  //it should "not decode MUL" in {
    //test(new Decoder) { c =>
     // c.io.inst.poke(assemble("mul, x1, x2, x3"))
      //c.io.ctrl.exception.peekBoolean() shouldBe true
    //}
  //}
  // ---------------------- M extension ---------------------- //
  it should "decode MUL" in {
    test(new Decoder) { c =>
      checkRType(c, "mul x1, x2, x3", AluOp.MUL, 1, 2, 3)
    }
  }
  it should "decode MULH" in {
    test(new Decoder) { c =>
      checkRType(c, "mulh x1, x2, x3", AluOp.MULH, 1, 2, 3)
    }
  }

  it should "decode MULHSU" in {
    test(new Decoder) { c =>
      checkRType(c, "mulhsu x1, x2, x3", AluOp.MULHSU, 1, 2, 3)
    }
  }

  it should "decode MULHU" in {
    test(new Decoder) { c =>
      checkRType(c, "mulhu x1, x2, x3", AluOp.MULHU, 1, 2, 3)
    }
  }

  it should "decode DIV" in {
    test(new Decoder) { c =>
      checkRType(c, "div x1, x2, x3", AluOp.DIV, 1, 2, 3)
    }
  }

  it should "decode DIVU" in {
    test(new Decoder) { c =>
      checkRType(c, "divu x1, x2, x3", AluOp.DIVU, 1, 2, 3)
    }
  }

  it should "decode REM" in {
    test(new Decoder) { c =>
      checkRType(c, "rem x1, x2, x3", AluOp.REM, 1, 2, 3)
    }
  }

  it should "decode REMU" in {
    test(new Decoder) { c =>
      checkRType(c, "remu x1, x2, x3", AluOp.REMU, 1, 2, 3)
    }
  }
  // ---------------------- M extension ---------------------- //
  it should "decode ADD" in {
    test(new Decoder) { c =>
      checkRType(c, "add x1, x2, x3", AluOp.ADD, 1, 2, 3)
    }
  }

  it should "decode ADDI" in {
    test(new Decoder) { c =>
      checkIType(c, "addi x1, x2, 3", AluOp.ADD, 1, 2, 3)
    }
  }

  it should "decode SUB" in {
    test(new Decoder) { c =>
      checkRType(c, "sub x1, x2, x3", AluOp.SUB, 1, 2, 3)
    }
  }

  it should "decode AND" in {
    test(new Decoder) { c =>
      checkRType(c, "and x1, x2, x3", AluOp.AND, 1, 2, 3)
    }
  }

  it should "decode ANDI" in {
    test(new Decoder) { c =>
      checkIType(c, "andi x1, x2, 3", AluOp.AND, 1, 2, 3)
    }
  }

  it should "decode SRA" in {
    test(new Decoder) { c =>
      checkRType(c, "sra x1, x2, x3", AluOp.SRA, 1, 2, 3)
    }
  }


  // See https://github.com/carlosedp/riscvassembler/issues/19.
  // it should "decode SRAI" in {
  //   test(new Decoder) { c =>
  //     println(RISCVAssembler.binOutput("srai x0, x0, 0"))
  //     checkIType(c, "srai x1, x2, 3", AluOp.SRA, 1, 2, 3)
  //   }
  // }

  it should "decode SRL" in {
    test(new Decoder) { c =>
      checkRType(c, "srl x1, x2, x3", AluOp.SRL, 1, 2, 3)
    }
  }

  it should "decode SRLI" in {
    test(new Decoder) { c =>
      checkIType(c, "srli x1, x2, 3", AluOp.SRL, 1, 2, 3)
    }
  }

  it should "decode SLL" in {
    test(new Decoder) { c =>
      checkRType(c, "sll x1, x2, x3", AluOp.SLL, 1, 2, 3)
    }
  }

  it should "decode SLLI" in {
    test(new Decoder) { c =>
      checkIType(c, "slli x1, x2, 3", AluOp.SLL, 1, 2, 3)
    }
  }

  it should "decode SLT" in {
    test(new Decoder) { c =>
      checkRType(c, "slt x1, x2, x3", AluOp.SLT, 1, 2, 3)
    }
  }

  it should "decode SLTU" in {
    test(new Decoder) { c =>
      checkRType(c, "sltu x1, x2, x3", AluOp.SLTU, 1, 2, 3)
    }
  }

  it should "decode SLTIU" in {
    test(new Decoder) { c =>
      checkIType(c, "sltiu x1, x2, 3", AluOp.SLTU, 1, 2, 3)
    }
  }

  it should "decode SLTI" in {
    test(new Decoder) { c =>
      checkIType(c, "slti x1, x2, 3", AluOp.SLT, 1, 2, 3)
    }
  }

  it should "decode OR" in {
    test(new Decoder) { c =>
      checkRType(c, "or x1, x2, x3", AluOp.OR, 1, 2, 3)
    }
  }

  it should "decode ORI" in {
    test(new Decoder) { c =>
      checkIType(c, "ori x1, x2, 3", AluOp.OR, 1, 2, 3)
    }
  }

  it should "decode XOR" in {
    test(new Decoder) { c =>
      checkRType(c, "xor x1, x2, x3", AluOp.XOR, 1, 2, 3)
    }
  }

  it should "decode XORI" in {
    test(new Decoder) { c =>
      checkIType(c, "xori x1, x2, 3", AluOp.XOR, 1, 2, 3)
    }
  }

  it should "decode JAL" in {
    test(new Decoder) { c =>
      checkJType(c, "jal x1, 124", AluOp.ADD, SpecialOp.JAL, 1, 124)
    }
  }

  it should "decode JALR" in {
    test(new Decoder) { c =>
      checkIType(c, "jalr x1, x2, 124", AluOp.ADD, 1, 2, 124, MemOp.NONE, SpecialOp.JALR)
    }
  }

  it should "decode LB" in {
    test(new Decoder) { c =>
      checkIType(c, "lb x1, 124(x2)", AluOp.ADD, 1, 2, 124, MemOp.LB)
    }
  }

  it should "decode LBU" in {
    test(new Decoder) { c =>
      checkIType(c, "lbu x1, 124(x2)", AluOp.ADD, 1, 2, 124, MemOp.LBU)
    }
  }

  it should "decode LH" in {
    test(new Decoder) { c =>
      checkIType(c, "lh x1, 124(x2)", AluOp.ADD, 1, 2, 124, MemOp.LH)
    }
  }

  it should "decode LHU" in {
    test(new Decoder) { c =>
      checkIType(c, "lhu x1, 124(x2)", AluOp.ADD, 1, 2, 124, MemOp.LHU)
    }
  }

  it should "decode LW" in {
    test(new Decoder) { c =>
      checkIType(c, "lw x1, 124(x2)", AluOp.ADD, 1, 2, 124, MemOp.LW)
    }
  }

  it should "decode SB" in {
    test(new Decoder) { c =>
      checkSType(c, "sb x1, 124(x2)", AluOp.ADD, 2, 1, 124, MemOp.SB)
    }
  }

  it should "decode SH" in {
    test(new Decoder) { c =>
      checkSType(c, "sh x1, 124(x2)", AluOp.ADD, 2, 1, 124, MemOp.SH)
    }
  }

  it should "decode SW" in {
    test(new Decoder) { c =>
      checkSType(c, "sw x1, 124(x2)", AluOp.ADD, 2, 1, 124, MemOp.SW)
    }
  }

  it should "decode BEQ" in {
    test(new Decoder) { c =>
      checkBType(c, "beq x1, x2, 124", AluOp.EQ, 1, 2, 124)
    }
  }

  it should "decode BNE" in {
    test(new Decoder) { c =>
      checkBType(c, "bne x1, x2, 124", AluOp.NE, 1, 2, 124)
    }
  }

  it should "decode BLT" in {
    test(new Decoder) { c =>
      checkBType(c, "blt x1, x2, 124", AluOp.LT, 1, 2, 124)
    }
  }

  it should "decode BLTU" in {
    test(new Decoder) { c =>
      checkBType(c, "bltu x1, x2, 124", AluOp.LTU, 1, 2, 124)
    }
  }

  it should "decode BGE" in {
    test(new Decoder) { c =>
      checkBType(c, "bge x1, x2, 124", AluOp.GE, 1, 2, 124)
    }
  }

  it should "decode BGEU" in {
    test(new Decoder) { c =>
      checkBType(c, "bgeu x1, x2, 124", AluOp.GEU, 1, 2, 124)
    }
  }

  it should "decode AUIPC" in {
    test(new Decoder) { c =>
      checkUType(
        c,
        "auipc x1, 0x123000",
        AluOp.NONE,
        SpecialOp.AUIPC,
        1,
        0x123 << 12,
      )
    }
  }

  it should "decode LUI" in {
    test(new Decoder) { c =>
      checkUType(
        c,
        "lui x1, 0x123000",
        AluOp.NONE,
        SpecialOp.LUI,
        1,
        0x123 << 12,
      )
    }
  }

  it should "decode FENCE" in {
    test(new Decoder) { c =>
      checkIType(c, "fence", AluOp.NONE, 0, 0, 0, MemOp.NONE, SpecialOp.FENCE)
    }
  }

  it should "decode EBREAK" in {
    test(new Decoder) { c =>
      checkIType(c, "ebreak", AluOp.NONE, 0, 0, 1, MemOp.NONE, SpecialOp.EBREAK)
    }
  }

  it should "decode ECALL" in {
    test(new Decoder) { c =>
      checkIType(c, "ecall", AluOp.NONE, 0, 0, 0, MemOp.NONE, SpecialOp.ECALL)
    }
  }

  def assemble(in: String): UInt = {
    if (in == "ebreak") {
      "b00000000000100000000000001110011".U
    } else if (in == "ecall") {
      "b00000000000000000000000001110011".U
    } else if (in == "fence") {
      "b00000000000000000000000000001111".U
    } else {
        in match {
        // ---------------------- M extension ---------------------- //
        case "mul x1, x2, x3"    => "h023100B3".U  // funct7=1, funct3=0, opcode=0x33
        case "mulh x1, x2, x3"   => "h023110B3".U(32.W)  // funct7=1, funct3=1
        case "mulhsu x1, x2, x3" => "h023120B3".U  // funct7=1, funct3=2
        case "mulhu x1, x2, x3"  => "h023130B3".U
        case "div x1, x2, x3"    => "h023140B3".U  // funct7=1, funct3=4
        case "divu x1, x2, x3"   => "h023150B3".U
        case "rem x1, x2, x3"    => "h023160B3".U
        case "remu x1, x2, x3"   => "h023170B3".U
        
        // ---------------------- M extension ---------------------- //
        // If it is not the above M command, it will be handed over to the original Assembler.
        // -------------------------------------------------
        case _ => ("b" + RISCVAssembler.binOutput(in)).U
      }

    }
  }

  def checkRType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      rs2: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe false
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.specialOp.peek() shouldBe SpecialOp.NONE
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
  }

  def checkIType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      imm: Int,
      memOp: MemOp.Type = MemOp.NONE,
      specialOp: SpecialOp.Type = SpecialOp.NONE,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe memOp
    c.io.ctrl.specialOp.peek() shouldBe specialOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.imm.peekInt() shouldBe imm
  }

  def checkJType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      specialOp: SpecialOp.Type,
      rd: Int,
      imm: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.specialOp.peek() shouldBe specialOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.imm.peekInt() shouldBe imm
  }

  def checkSType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rs1: Int,
      rs2: Int,
      offset: Int,
      memOp: MemOp.Type = MemOp.NONE,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe memOp
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
    c.io.ctrl.imm.peekInt() shouldBe offset
  }

  def checkBType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rs1: Int,
      rs2: Int,
      offset: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe true
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.specialOp.peek() shouldBe SpecialOp.NONE
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
    c.io.ctrl.imm.peekInt() shouldBe offset
  }

  def checkUType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      specialOp: SpecialOp.Type,
      rd: Int,
      imm: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isBranch.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.specialOp.peek() shouldBe specialOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.imm.peekInt() shouldBe imm
  }
}