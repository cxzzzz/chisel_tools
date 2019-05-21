
package tools

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO, TransitName}

class MultDecoupledIO[T <: Data](gen: T ,entries:Int) extends Module{

  val io = IO( new Bundle {

    val in = Flipped(Decoupled(gen))
    val out = Vec(entries , Decoupled(gen))
  }
  )

  val recvRegs=Seq.fill(entries){ RegInit(false.B) }

  io.in.ready := io.out.zip( recvRegs)
    .map( x =>  x._1.ready || x._2)
    .reduce( _ && _)

  for( idx <- 0 until entries){

    recvRegs(idx):= switch(io.in.ready){ false.B}
      .otherwise( recvRegs(idx) || (io.out(idx).ready && io.in.valid))

    io.out(idx).bits := io.in.bits
    io.out(idx).valid := io.in.valid && (!recvRegs(idx))

  }

}

class MultDecoupledIOWithEn[T <:Data](gen:T,entries:Int) extends  Module{

  val io = IO( new Bundle {

    val in =  Flipped(Decoupled(gen))
    val en = Vec( entries, Input(Bool()))
    val out = Vec(entries , Decoupled(gen))
  }
  )

  val multDecoupled = MultDecoupled( io.in, entries)

  for(idx <- 0 until entries){
    io.out(idx).valid := io.en(idx) && multDecoupled(idx).valid
    io.out(idx).bits := multDecoupled(idx).bits
    multDecoupled(idx).ready :=  (!io.en(idx)) || io.out(idx).ready
   }


}

object MultDecoupled
{

  def apply[T <:Data](enq:DecoupledIO[T],entries:Int): Vec[DecoupledIO[T]]={
    require(entries>1)
    val m :MultDecoupledIO[T]= Module(new MultDecoupledIO( chiselTypeOf(enq.bits),entries ))
    m.io.in <> enq
    TransitName(m.io.out , m)
  }
}

object MultDecoupledWithEn
{

  def apply[T <:Data](enq:DecoupledIO[T],enables:Seq[Bool],entries:Int): Vec[DecoupledIO[T]]={
    require(entries>1)
    val m :MultDecoupledIOWithEn[T]= Module(new MultDecoupledIOWithEn[T]( chiselTypeOf(enq.bits),entries ))
    m.io.in <> enq
    m.io.en.zip(enables).foreach( x=> x._1 := x._2)
    TransitName(m.io.out , m)
  }
}










