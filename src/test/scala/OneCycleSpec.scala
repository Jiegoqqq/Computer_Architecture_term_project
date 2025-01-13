package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import com.carlosedp.riscvassembler.RISCVAssembler

import chisel3._
import chiseltest._

class OneCycleSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "OneCycle"

  def waitLoaded(c: OneCycleSim) = {
    while (!c.test.loaded.peekBoolean()) {
      c.clock.step()
    }
  }
  // ---------------------- M extension ---------------------- //
  def assemble(in: String): Int = {
   in match {

      case "mul x1, x2, x3" => Integer.parseUnsignedInt("023100B3", 16)
      case "mulh x1, x2, x3" => Integer.parseUnsignedInt("023110B3", 16)
      case "mulhsu x1, x2, x3" => Integer.parseUnsignedInt("023120B3", 16)
      case "mulhu x1, x2, x3" => Integer.parseUnsignedInt("023130B3", 16)
      case "div x1, x2, x3" => Integer.parseUnsignedInt("023140B3", 16)
      case "divu x1, x2, x3" => Integer.parseUnsignedInt("023150B3", 16)
      case "rem x1, x2, x3" => Integer.parseUnsignedInt("023160B3", 16)
      case "remu x1, x2, x3" => Integer.parseUnsignedInt("023170B3", 16)

      case "nop" => Integer.parseUnsignedInt("00000013", 16)

      case "ebreak" => Integer.parseUnsignedInt("00000000000000000000000001110011", 2)
      case "ecall" => Integer.parseUnsignedInt("00000000000100000000000001110011", 2)
      case "fence" => Integer.parseUnsignedInt("00000000000000000000000000001111", 2)

      case other =>
        val binStr = RISCVAssembler.binOutput(other)
        Integer.parseUnsignedInt(binStr, 2)
    }
  // ---------------------- M extension ---------------------- //
  
  }

  def peekReg(c: OneCycleSim, reg: Int): BigInt = {
    c.test.regs.readAddr.poke(reg.U)
    c.test.regs.readData.peekInt()
  }

  it should "halt on invalid instruction" in {
    test(new OneCycleSim(List(0))) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  it should "halt on misaligned memory access" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 2"),
          assemble("jalr x0, x1, 0"),
          assemble("add x0, x0, x0"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe false
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  it should "keep asserting halt" in {
    test(new OneCycleSim(List(0, assemble("add x1, x2, x3"))))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        waitLoaded(c)
        for (i <- 0 until 10) {
          c.signals.halted.peekBoolean() shouldBe true
          c.clock.step()
        }
      }
  }

  it should "not halt on valid instruction" in {
    test(new OneCycleSim(List(assemble("add x1, x2, x3")))) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
    }
  }

  it should "handle jal" in {
    test(
      new OneCycleSim(
        List(
          assemble("add x0, x0, x0"),
          assemble("jal x1, -4"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
      peekReg(c, 1) shouldBe 8
    }
  }

  it should "handle jalr" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x3, x0, 8"),
          assemble("jalr x1, x3, 4"),
          assemble("addi x0, x0, 0"),
          assemble("addi x0, x0, 0"),
          assemble("addi x0, x0, 0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 12
      peekReg(c, 1) shouldBe 8
    }
  }

  it should "handle writes to registers" in {
    test(
      new OneCycleSim(List(assemble("addi x1, x0, 5"))),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
    }
  }

  it should "handle reads from registers" in {
    test(
      new OneCycleSim(
        List(assemble("addi x1, x0, 5"), assemble("addi x2, x1, 3")),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
      c.clock.step()
      peekReg(c, 2) shouldBe 8
    }
  }

  it should "handle reads and writes to the same register" in {
    test(
      new OneCycleSim(
        List(assemble("addi x1, x0, 3"), assemble("addi x1, x1, 7")),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 3
      c.clock.step()
      peekReg(c, 1) shouldBe 10
    }
  }

  it should "handle writes to memory" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 5"),
          assemble("sw x1, 9(x0)"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
      c.dmem.memOp.peek() shouldBe MemOp.SW
      c.dmem.addr.peekInt() shouldBe 9
      c.dmem.writeData.peekInt() shouldBe 5
    }
  }

  it should "handle reads from memory" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 5"),
          assemble("sw x1, 16(x0)"),
          assemble("lw x2, 11(x1)"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step()
      c.dmem.memOp.peek() shouldBe MemOp.LW
      c.dmem.addr.peekInt() shouldBe 16
      c.dmem.readData.poke(5)
      c.clock.step()
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "handle auipc" in {
    test(
      new OneCycleSim(
        List(
          assemble("auipc x1, 0x1000"),
          assemble("auipc x2, 0x123000"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 0x1000
      c.clock.step()
      peekReg(c, 2) shouldBe 0x123004
    }
  }

  it should "handle lui" in {
    test(
      new OneCycleSim(
        List(
          assemble("lui x1, 0x1000"),
          assemble("lui x2, 0x123000"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 0x1000
      c.clock.step()
      peekReg(c, 2) shouldBe 0x123000
    }
  }

  it should "handle backwards conditional branches" in {
    // Backward branches
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 1"),
          assemble("beq x1, x0, -4"),
          assemble("beq x1, x1, -8"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
    }
  }

  it should "handle forwards conditional branches" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 1"),
          assemble("blt x0, x1, 4"),
          assemble("blt x0, x1, 8"),
          assemble("add x0, x0, x0"),
          assemble("add x0, x0, x0"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 16
      c.clock.step()
      c.test.pc.peekInt() shouldBe 20
    }
  }

  it should "ignore ecall, ebreak and fence" in {
    test(
      new OneCycleSim(
        List(
          assemble("fence"),
          assemble("ecall"),
          assemble("ebreak"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 12
    }
  }
  // ---------------------- M extension ---------------------- //
  //MUL test
  it should "handle mul" in {
    test(
      new OneCycleSim(
        List(
          // x2 = 2
          assemble("addi x2, x0, 2"),
          // x3 = 3
          assemble("addi x3, x0, 3"),
          // x1 = x2 * x3 = 6
          assemble("mul x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 2  // x2 = 2

      c.clock.step()
      peekReg(c, 3) shouldBe 3  // x3 = 3

      c.clock.step()
      peekReg(c, 1) shouldBe 6  // x1 = 6

      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  // MULH test
  it should "handle mulh " in {
    test(
      new OneCycleSim(
        List(
          // x2 = 1
          assemble("addi x2, x0, 1"),
          // x2 = x2 << 16 = 65536
          assemble("slli x2, x2, 16"),

          // x3 = 1
          assemble("addi x3, x0, 1"),
          // x3 = x3 << 16 = 65536
          assemble("slli x3, x3, 16"),

          // high 32 bits
          // (65536 * 65536) >> 32 = 1
          assemble("mulh x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      // 1 ：addi x2, x0, 1
      c.clock.step()
      peekReg(c, 2) shouldBe 1      // x2 = 1

      // 2 ：slli x2, x2, 14
      c.clock.step()
      peekReg(c, 2) shouldBe 65536  // x2 = 65536

      // 3 ：addi x3, x0, 1
      c.clock.step()
      peekReg(c, 3) shouldBe 1      // x3 = 1

      // 4 ：slli x3, x3, 14
      c.clock.step()
      peekReg(c, 3) shouldBe 65536  // x3 = 65536

      // 5 ：mulh x1, x2, x3
      c.clock.step()
      peekReg(c, 1) shouldBe 1      // high 32 bits = 1
      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  //MULHSU
  it should "handle mulhsu" in {
    test(
      new OneCycleSim(
        List(
          // x2 = 1
          assemble("addi x2, x0, 1"),
          // x2 = x2 << 16 = 65536
          assemble("slli x2, x2, 16"),

          // x3 = 1
          assemble("addi x3, x0, 1"),
          // x3 = x3 << 16 = 65536
          assemble("slli x3, x3, 16"),

          // high 32 bits
          // (65536 * 65536) >> 32 = 1
          assemble("mulhsu x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      // 1：addi x2, x0, 1
      c.clock.step()
      peekReg(c, 2) shouldBe 1

      // 2：slli x2, x2, 16
      c.clock.step()
      peekReg(c, 2) shouldBe 65536

      // 3：addi x3, x0, 1
      c.clock.step()
      peekReg(c, 3) shouldBe 1

      // 4：slli x3, x3, 16
      c.clock.step()
      peekReg(c, 3) shouldBe 65536

      // 5：mulhsu x1, x2, x3
      c.clock.step()
      peekReg(c, 1) shouldBe 1  // high 32 bits = 1

      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  //MULHU
  it should "handle mulhu" in {
    test(
      new OneCycleSim(
        List(
          // x2 = 1
          assemble("addi x2, x0, 1"),
          // x2 = x2 << 16 = 65536
          assemble("slli x2, x2, 16"),

          // x3 = 1
          assemble("addi x3, x0, 1"),
          // x3 = x3 << 16 = 65536
          assemble("slli x3, x3, 16"),

          // (65536 * 65536) >> 32 = 1
          assemble("mulhu x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 1

      c.clock.step()
      peekReg(c, 2) shouldBe 65536

      c.clock.step()
      peekReg(c, 3) shouldBe 1

      c.clock.step()
      peekReg(c, 3) shouldBe 65536

      c.clock.step()
      peekReg(c, 1) shouldBe 1  // high 32 bits = 1

      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  //DIV test
  it should "handle div" in {
    test(
      new OneCycleSim(
        List(
          // x2 = 10
          assemble("addi x2, x0, 10"),
          // x3 = 3
          assemble("addi x3, x0, 3"),
          // div x1, x2, x3 => 10 / 3 = 3 
          assemble("div x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 10

      c.clock.step()
      peekReg(c, 3) shouldBe 3

      c.clock.step()
      peekReg(c, 1) shouldBe 3

      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  //DIVU test
  it should "handle divu" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x2, x0, 10"),
          assemble("addi x3, x0, 3"),
          // divu x1, x2, x3 => 10 / 3 = 3 
          assemble("divu x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 10

      c.clock.step()
      peekReg(c, 3) shouldBe 3

      c.clock.step()
      peekReg(c, 1) shouldBe 3

      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  //REM test
  it should "handle rem" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x2, x0, 10"),
          assemble("addi x3, x0, 3"),
          // rem x1, x2, x3 => 10 % 3 = 1 
          assemble("rem x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 10

      c.clock.step()
      peekReg(c, 3) shouldBe 3

      c.clock.step()
      peekReg(c, 1) shouldBe 1

      c.signals.halted.peekBoolean() shouldBe true
    }
  }
  //REMU test
  it should "handle remu" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x2, x0, 10"),
          assemble("addi x3, x0, 3"),
          // remu x1, x2, x3 => 10 % 3 = 1 
          assemble("remu x1, x2, x3"),
        ),
      ),
    ) { c =>
      waitLoaded(c)

      c.clock.step()
      peekReg(c, 2) shouldBe 10

      c.clock.step()
      peekReg(c, 3) shouldBe 3

      c.clock.step()
      peekReg(c, 1) shouldBe 1

      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  // ---------------------- M extension ---------------------- //
}