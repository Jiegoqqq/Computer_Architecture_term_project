package mrv

import chisel3._
import chisel3.util._
/**
 * RV32IM: 32 registers x0~x31, where x0 = 0
 * Mem(32, UInt(32.W)) is used here to represent 32 32-bit temporary registers.
 */
class RegFile extends Module {
  val io = IO(new Bundle {
    // 5 bits address (0~31)
    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val rd  = Input(UInt(5.W))

    val writeEnable = Input(Bool())
    val writeData   = Input(UInt(32.W))

    val rs1Data = Output(UInt(32.W))
    val rs2Data = Output(UInt(32.W))
  })

  val test = IO(new Bundle {
    val reg     = Input(UInt(5.W))
    val regData = Output(UInt(32.W))
  })

  // 32 32-bit registers
  val regs = Mem(32, UInt(32.W))
  
  // Default output
  io.rs1Data   := 0.U
  io.rs2Data   := 0.U
  test.regData := 0.U
  // Read rs1
  when(io.rs1 =/= 0.U) {
    io.rs1Data := regs.read(io.rs1)
    // printf("reading 0x%x from rs1=0x%x\n", io.rs1Data, io.rs1.bits)
  }
  // Read rs2
  when(io.rs2 =/= 0.U) {
    io.rs2Data := regs.read(io.rs2)
    // printf("reading 0x%x from rs2=0x%x\n", io.rs2Data, io.rs2.bits)
  }
  // test read
  when(test.reg =/= 0.U) {
    test.regData := regs.read(test.reg)
  }
  // write rd
  when(io.writeEnable) {
    regs.write(io.rd, io.writeData)
    // printf("writing 0x%x to rd=0x%x\n", io.rdData, io.rd.bits)
  }
}
