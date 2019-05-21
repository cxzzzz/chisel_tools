
package tools

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.{Decoupled, DecoupledIO, Queue}


class WeightedDecoupledBit[T <:Data](gen:T,weightWidth:Width) extends Bundle{
  val weight = UInt(weightWidth)
  val end = Bool()
  val bits = gen.cloneType.asInstanceOf[T]
  override def cloneType: this.type = new WeightedDecoupledBit(gen,weightWidth).asInstanceOf[this.type]
}

class WeightedDecoupledIO[T <:Data](gen:T ,weightWidth:Width) extends DecoupledIO(new WeightedDecoupledBit(gen,weightWidth )){

    override def cloneType: this.type = new WeightedDecoupledIO(gen,weightWidth).asInstanceOf[this.type]
}

object WeightedDecoupled
{
  def apply[T <: Data](gen:T,weightWidth:Width):WeightedDecoupledIO[T] = new WeightedDecoupledIO[T](gen,weightWidth)
}

class WeightedArbiter[T <: Data](gen: WeightedDecoupledIO[T], n:Int)  extends  Module{


    val io = IO( new Bundle{
      val in =Vec(n,Flipped(gen))
      val out = gen
      }
    )

  val inReadyTmps = Seq.fill(n){ Wire(Bool())}

  val endOutRegs = Seq.fill(n){ RegInit(false.B) }

  val allEndOut = endOutRegs.zip(io.in).zip(inReadyTmps).map( x=> (x._1._1,x._1._2,x._2))
    .map( x=> x._1 || ( x._2.valid && x._2.bits.end && x._3) )
    .reduce( _ && _ )

  /*val minWeight = io.in.zip(endOutRegs).reduce((x1,x2)=> switch(!(x1._2 || x2._2)){
      switch(ULt(x1._1.weight,x2._1.weight)){x1}
        .otherwise(x2)
    }.elseswitch(x1._2){ x1 }
    .otherwise{ x2 })._1.weight
    */


  /*
  class tmpBund[T <: Data](_end:Bool,_weight:UInt,_bits:T) extends Bundle{
    val end = Bool()
    val weight = _weight.cloneType
    val bits = _bits.cloneType

    override def cloneType: this.type = new tmpBund(_end,_weight,_bits).asInstanceOf[this.type]
  }
*/

  val minWeight = io.in.zip(endOutRegs)
      .map( x=> {
        val t = Wire(new WeightedDecoupledBit(x._1.bits.bits,x._1.bits.weight.getWidth.W))
        t := x._1.bits
        t.end := x._2
        t
        }
      )
      .reduce( (x1,x2)=> switch(!(x1.end || x2.end)){
        switch(ULt(x1.weight,x2.weight)){x1}
          .otherwise{x2}
      }.elseswitch{x1.end}{ x2 }
        .otherwise{ x1 }).weight



  io.out.valid := io.in.map( _.valid).reduce( _ && _)

  io.out.bits.bits := io.in.map( x => switch(x.bits.weight === minWeight){ x.bits.bits}.otherwise{ 0.U.asTypeOf(x.bits.bits) })
    .reduce( (x1:T,x2:T)=> (x1.asUInt() | x2.asUInt()).asTypeOf(io.out.bits.bits))

  io.out.bits.weight := minWeight

  io.out.bits.end :=  io.in.zip(endOutRegs).map( x=> ((x._1.bits.weight=== minWeight) && (x._1.bits.end))|| x._2).reduce(_ && _)
  //io.in.zip(endOutRegs).( _.bits.end).reduce( _ && _ )//allEndOut

  for( idx<- 0 until n){
    io.in(idx).ready := switch(io.in(idx).bits.end){allEndOut}.otherwise{inReadyTmps(idx)&& io.out.valid}

    inReadyTmps(idx) := io.out.ready &&  io.in(idx).bits.weight === minWeight

    endOutRegs(idx) := switch( allEndOut ){ false.B }
      .elseswitch( io.in(idx).bits.end && io.in(idx).valid && inReadyTmps(idx)){true.B}
      .otherwise( endOutRegs(idx))

  }

  def cloneType: this.type = new WeightedArbiter(gen,n).asInstanceOf[this.type]

}


object WeightedArbiter extends App{

  def apply[T <:Data ] ( gen:WeightedDecoupledIO[T],n:Int): WeightedArbiter[T]={
      val genType=gen.cloneType.asInstanceOf[WeightedDecoupledIO[T]]
       Module(new WeightedArbiter[T](genType,n))
  }

  def main: Unit = {
    //Driver.elaborate(()=>new DensePe(8,8,16))
    //val a: String = Driver.emitVerilog(new DenseSa(new DefaultParameter))
    class btest extends Bundle{
      val value= UInt(8.W)
      override def cloneType: this.type = new btest().asInstanceOf[this.type]

    }
    val io =WeightedDecoupled( new btest(), 8.W)

    val a: String = Driver.emitVerilog(WeightedArbiter(io,3))
    print(a)
  }
  main

}


