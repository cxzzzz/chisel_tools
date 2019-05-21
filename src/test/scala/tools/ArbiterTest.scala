
package tools

import java.util.Random

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util.Queue

class WeightedArbiterWithFifo(val n:Int) extends Module{

    val io = IO( new Bundle{
        val in= Vec(n,Flipped(WeightedDecoupled(UInt(8.W),8.W)))
        val out = WeightedDecoupled(UInt(8.W),8.W)
    })

    val inFifos= io.in.map(x => Queue(x,3000))

    val weightedArbiter =WeightedArbiter(WeightedDecoupled(UInt(8.W),8.W),n)

    inFifos.zip(weightedArbiter.io.in).foreach(x =>{

        x._1 <> x._2
        //x._1.ready := x._2.ready
        //x._2.valid := x._1.valid
        //x._2.bits := x._1.bits

    })

    val outFifo=Queue(weightedArbiter.io.out,3000)

    io.out <> outFifo
    //io.out.valid :=  outFifo.valid
    //io.out.bits := outFifo.bits
    //outFifo.ready := io.out.ready


}
trait WeightedArbiterTestTools{

    //value ,weight,end
    def createOutput(r:Random)(maxLen:Int,sparsity:Double): List[(Int,Int,Boolean)]={
        val denseOutputTmp= List.fill(maxLen){ r.nextInt( 1<< 8)}.zip( (0 until maxLen).map( _%(1<<8)) )

        val sparseOutputTmp ={
            def acc( o:List[(Int,Int)]):List[(Int,Int)]={
                if( o.isEmpty){ o }
                else{
                    if (r.nextInt(10000)<sparsity*10000)o.head :: acc(o.tail)
                    else acc(o.tail)
                }
            }
            acc( denseOutputTmp)
        }

       sparseOutputTmp.zip( (0 until sparseOutputTmp.length).map(x=>if(x<sparseOutputTmp.length-1)false else true))
          .map(x=> (x._1._1,x._1._2,x._2))
    }

    def createInputs(r:Random)(output:List[(Int,Int,Boolean)],n:Int): List[List[(Int,Int,Boolean)]]={
        val rate= 1.0/n+0.4
        val inputsTmp= List.fill(n-1){
            def acc(o:List[(Int,Int,Boolean)]):List[(Int,Int,Boolean)]={
                if( o.isEmpty){ o }
                else{
                    if (r.nextInt(10000)<rate*10000)o.head :: acc(o.tail)
                    else acc(o.tail)
                }
            }
            acc(output)
        }

        def acc1( o:List[(Int,Int,Boolean)]):List[(Int,Int,Boolean)]={
            if(o.length==1){
                (o.head._1,o.head._2,true)::Nil
            }
            else{
                o.head::acc1(o.tail)
            }
        }

        val inputsTail=output.filter( x=> ! inputsTmp.exists( in=> in.contains(x) ) )

        (inputsTmp ++ (inputsTail::Nil)).map( x=> acc1(x))

    }

}

class Base1WeightedArbiterTest(c:WeightedArbiterWithFifo) extends PeekPokeTester(c) with WeightedArbiterTestTools {

    val r = new Random(1234)
    val cycles = 1
    val maxLen = 800

    for( k<- 0 until cycles ){
        val output= createOutput(r)(maxLen,1.1)
        val inputs= createInputs(r)(output,c.n)
        println(output.toString())
        println(inputs.toString())

        for(i<-0 until maxLen + 2){
            for(j <- 0 until c.n){
                if( inputs(j).length > i) {
                    poke(c.io.in(j).valid, true)
                    poke(c.io.in(j).bits.bits, inputs(j)(i)._1)
                    poke(c.io.in(j).bits.weight, inputs(j)(i)._2)
                    poke(c.io.in(j).bits.end, inputs(j)(i)._3)
                }
                else{
                    poke(c.io.in(j).valid,false)
                }
            }
            step(1)
        }
        step( maxLen*2+100)

        for(i<-0 until output.length){
            poke(c.io.out.ready,true)
            expect(c.io.out.valid,true)
            expect(c.io.out.bits.bits,output(i)._1)
            expect(c.io.out.bits.weight,output(i)._2)
            expect(c.io.out.bits.end,output(i)._3)
            step(1)
        }
        poke(c.io.out.ready,false)
    }
}
