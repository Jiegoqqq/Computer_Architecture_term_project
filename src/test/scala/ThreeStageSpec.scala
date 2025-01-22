package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import com.carlosedp.riscvassembler.RISCVAssembler
import chisel3._
import chiseltest._

class ThreeStageSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "ThreeStageCPU"

  /** 
    * 等待記憶體載入完成（MemorySim 在初始化指令時會拉 high "loaded"）
    * 若 "loaded" 仍為 false，代表記憶體還沒載入完畢，就先暫停測試。
    */
  def waitLoaded(c: ThreeStageSim): Unit = {
    while (!c.test.loaded.peekBoolean()) {
      c.clock.step()
    }
  }

  /**
    * 用來組譯單行文字（例如 "add x1, x2, x3"）成為機器碼。
    * 以下範例同 OneCycleSpec 的處理，優先用手刻的 hex 或 bin，
    * 若都沒有，則委由 RISCVAssembler.binOutput(other) 做組譯。
    */
  def assemble(in: String): Int = {
    in match {
      // 這些指令範例與 OneCycleSpec 相同，只是示範如何直接回傳 32-bit 常數
      case "mul x1, x2, x3"   => Integer.parseUnsignedInt("023100B3", 16)
      case "mulh x1, x2, x3"  => Integer.parseUnsignedInt("023110B3", 16) // 修正編碼
      case "mulhsu x1, x2, x3"=> Integer.parseUnsignedInt("023120B3", 16)
      case "mulhu x1, x2, x3" => Integer.parseUnsignedInt("023130B3", 16)
      case "div x1, x2, x3"   => Integer.parseUnsignedInt("023140B3", 16)
      case "divu x1, x2, x3"  => Integer.parseUnsignedInt("023150B3", 16)
      case "rem x1, x2, x3"   => Integer.parseUnsignedInt("023160B3", 16)
      case "remu x1, x2, x3"  => Integer.parseUnsignedInt("023170B3", 16)

      case "nop"    => Integer.parseUnsignedInt("00000013", 16)

      case "ebreak" => Integer.parseUnsignedInt("00100073", 16) // 修正編碼
      case "ecall"  => Integer.parseUnsignedInt("00000073", 16)
      case "fence"  => Integer.parseUnsignedInt("0000000F", 16)

      // 其他指令就交由 RISCVAssembler 幫我們組譯
      case other =>
        val binStr = RISCVAssembler.binOutput(other)
        Integer.parseUnsignedInt(binStr, 2)
    }
  }

  /** 
    * 幫助我們讀取指定暫存器的值。
    * 先對 "readAddr" poke，然後 peek "readData"。
    */
  def peekReg(c: ThreeStageSim, reg: Int): BigInt = {
    c.test.regs.readAddr.poke(reg.U)
    c.test.regs.readData.peekInt()
  }

  it should "load instructions (simple test: read x1 after addi x1, x0, 5)" in {
    test(new ThreeStageSim(List(assemble("addi x1, x0, 5")))) { c =>
      // 1. 等待記憶體載入
      waitLoaded(c)

      // 2. 初始時 PC=0，跑一個 clock 執行指令 "addi x1, x0, 5"
      c.clock.step()

      // 3. 寫回 (WB) 通常在第三或第四個 clock 才會更新到 regFile
      //    由於有 pipeline，因此要再多 step 幾下
      c.clock.step(2)

      // 4. 檢查 x1 的值是否為 5
      peekReg(c, 1) shouldBe 5
    }
  }

  it should "handle sequential instructions (addi x1=5, addi x2=3 => x2=3, x1=5)" in {
    test(new ThreeStageSim(List(
      assemble("addi x1, x0, 5"),
      assemble("addi x2, x0, 3")
    ))) { c =>
      waitLoaded(c)

      // 1：addi x1, x0, 5
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5

      // 2：addi x2, x0, 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 3
    }
  }

  it should "write then read from the same register (RAW hazard) - (addi x1=10, addi x2, x1, 5 => x2=15)" in {
    test(new ThreeStageSim(List(
      assemble("addi x1, x0, 10"),
      assemble("addi x2, x1, 5") // x2 = x1 + 5
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 10

      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 15
    }
  }

  it should "support memory load/store" in {
    test(new ThreeStageSim(List(
      // x1 = 0x5
      assemble("addi x1, x0, 5"),
      // mem[100] = x1
      assemble("sw x1, 100(x0)"),
      // x2 = mem[100]
      assemble("lw x2, 100(x0)")
    ))) { c =>
      waitLoaded(c)

      // addi x1, x0, 5
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5

      // sw x1, 100(x0)
      c.clock.step()
      c.clock.step(2)
      c.clock.step(1)

      // lw x2, 100(x0)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "handle writes to registers" in {
    test(new ThreeStageSim(List(assemble("addi x1, x0, 5")))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5
    }
  }

  it should "handle reads from registers" in {
    test(new ThreeStageSim(List(assemble("addi x1, x0, 5"), assemble("addi x2, x1, 3")))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 8
    }
  }

  it should "handle writes to memory" in {
    test(new ThreeStageSim(List(assemble("addi x1, x0, 5"), assemble("sw x1, 9(x0)")))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5
      c.clock.step()
      c.clock.step(2)
    }
  }

  it should "handle reads from memory" in {
    test(new ThreeStageSim(List(
      assemble("addi x1, x0, 5"),
      assemble("sw x1, 16(x0)"),
      assemble("lw x2, 16(x0)")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 5
      c.clock.step()
      c.clock.step(2)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "ignore ecall, ebreak and fence" in {
    test(new ThreeStageSim(List(
      assemble("fence"),
      assemble("ecall"),
      assemble("ebreak"),
      assemble("add x0, x0, x0")
    ))) { c =>
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

  it should "handle mul" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 2"),
      assemble("addi x3, x0, 3"),
      assemble("mul x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 2
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 6
    }
  }

  it should "handle mulh" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 1"),
      assemble("slli x2, x2, 16"),
      assemble("addi x3, x0, 1"),
      assemble("slli x3, x3, 16"),
      assemble("mulh x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 1
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 1
    }
  }

  it should "handle mulhsu" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 1"),
      assemble("slli x2, x2, 16"),
      assemble("addi x3, x0, 1"),
      assemble("slli x3, x3, 16"),
      assemble("mulhsu x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 1
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 1
    }
  }

  it should "handle mulhu" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 1"),
      assemble("slli x2, x2, 16"),
      assemble("addi x3, x0, 1"),
      assemble("slli x3, x3, 16"),
      assemble("mulhu x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 1
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 65536
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 1
    }
  }  

  it should "handle div" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 10"),
      assemble("addi x3, x0, 3"),
      assemble("div x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 10
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 3
    }
  }

  it should "handle divu" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 10"),
      assemble("addi x3, x0, 3"),
      assemble("divu x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 10
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 3
    }
  }

  it should "handle rem" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 10"),
      assemble("addi x3, x0, 3"),
      assemble("rem x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 10
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 1
    }
  }

  it should "handle remu" in {
    test(new ThreeStageSim(List(
      assemble("addi x2, x0, 10"),
      assemble("addi x3, x0, 3"),
      assemble("remu x1, x2, x3")
    ))) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 2) shouldBe 10
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 3) shouldBe 3
      c.clock.step()
      c.clock.step(2)
      peekReg(c, 1) shouldBe 1
    }
  }
  
  it should "Quiz test 1 (handle absolute value)" in {
    test(new ThreeStageSim(List(
      // x1 = -5
      assemble("addi x1, x0, -5"),
      // x1 = 0 - x1 (即 x1 = 5)
      assemble("sub x1, x0, x1"),
      // x2 = x1 (即 x2 = 5)
      assemble("add x2, x1, x0")
    ))) { c =>
      waitLoaded(c)
      // 給 pipeline 足夠的時鐘週期，確保所有指令完成
      c.clock.step(6)

      // 此時 x1、x2 都應該是 5
      peekReg(c, 1) shouldBe 5
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "Quiz test 2 (n*m via mul) " in {
    // 我們測試 n=1..7, m=1..7 共 49 種組合
    for (n <- 1 to 7; m <- 1 to 7) {
      test(new ThreeStageSim(List(
        // x2 <- n
        assemble(s"addi x2, x0, $n"),
        // x3 <- m
        assemble(s"addi x3, x0, $m"),
        // x1 = x2 * x3
        assemble("mul x1, x2, x3")
      ))) { c =>
        waitLoaded(c)

        // 第1條指令：addi x2, x0, n
        c.clock.step()
        c.clock.step(2)
        peekReg(c, 2) shouldBe n

        // 第2條指令：addi x3, x0, m
        c.clock.step()
        c.clock.step(2)
        peekReg(c, 3) shouldBe m

        // 第3條指令：mul x1, x2, x3
        c.clock.step()
        c.clock.step(2)
        peekReg(c, 1) shouldBe (n*m)
      }
    }
  }
  def assembleAll(src: String): List[Int] = {
    // 讓 RISCVAssembler 一次解析整段含 label 的程式
    val binLines = RISCVAssembler.binOutput(src) 
    // binLines 會是一串多行的 "101100..." (二進位字串)，每行對應一條指令
    binLines.split("\n").map { binStr =>
      // 轉成 32-bit 整數
      Integer.parseUnsignedInt(binStr, 2)
    }.toList
  }
  it should "Quiz test 2 (logint)" in {
    val code =
      """addi x2, x0, 16   # x2 = N=16
        |addi x3, x0, 0    # x3 = i=0
        |
        |loop:
        |  beq x2, x0, end  # if (x2 == 0) => 跳到 end
        |  srai x2, x2, 1   # x2 >>= 1
        |  addi x3, x3, 1   # i++
        |  jal x0, loop     # 無條件跳回 loop
        |
        |end:
        |  addi x4, x3, -1  # x4 = i - 1
        |  nop
        |""".stripMargin

    // 1. 一次組譯多行（含標籤）的程式
    val initInstrs = assembleAll(code)

    // 2. 建立模組並測試
    test(new ThreeStageSim(initInstrs)) { c =>
      // 先等記憶體載入
      waitLoaded(c)

      // 跑更多 clock，讓整個迴圈執行完畢並更新 x4
      c.clock.step(40)
      
      // 檢查 x4，預期得到 4
      peekReg(c, 2) shouldBe 4
    }
  }


}
