
package tools

import chisel3._
import chisel3.core.requireIsHardware
import chisel3.internal.firrtl.Width

//unsigned comparator

class ULt(width:Int) extends  Module{
  val io = IO( new Bundle {
    val op1 = Input(UInt(width.W))
    val op2 = Input(UInt(width.W))
    val Lt = Output(Bool())
  }
  )

  io.Lt := (io.op1-io.op2) >  (1<<( width-1)).U

}

object ULt{
  def apply( op1:UInt,op2:UInt):Bool={

    requireIsHardware(op1, "op1")
    requireIsHardware(op2, "op2")
    require( op1.getWidth == op2.getWidth)

    val ult=Module( new ULt( op1.getWidth ) )
    ult.io.op1 := op1
    ult.io.op2 := op2

    ult.io.Lt

  }

}

class UMin(width:Width) extends Module{
  val io= IO( new Bundle{
    val op1=Input(UInt(width))
    val op2=Input(UInt(width))
    val min = Output(UInt(width))
  })
  io.min := switch( ULt(io.op1,io.op2) ){ io.op1}.otherwise{io.op2}
}

object UMin{
  def apply(op1:UInt,op2:UInt):UInt={
    requireIsHardware(op1, "op1")
    requireIsHardware(op2, "op2")
    require( op1.getWidth == op2.getWidth)

    val umin=Module( new UMin( op1.getWidth.W) )
    umin.io.op1 := op1
    umin.io.op2 := op2
    umin.io.min
  }
}
