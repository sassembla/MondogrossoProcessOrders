package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith



@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {
	
	"parseOrder parse" should {
		val input = "A>B,C,D(A:a:c)>E(D:d:e),F(A:a2:f, B:b:f2)<J,K>I(J:j:i)+D>G>H+G>J!Z"
			/*processes =
			 * A>B,C,D(A:a:c)>E(D:d:e),F(A:a2:f, B:b:f2)<J,K>I(J:j:i)
			 *  +D>G>H
			 *  +G>J
			 */
			
			/*finally = !
			 * !Z
			 */
			
			/*process = >
			 * process 1:
			 * A
			 * B,C,D(A:a:c)
			 * E(D:d:e),F(A:a2:f, B:b:f2)<J,K
			 * I(J:j:i)
			 * 
			 * process 2:
			 * G
			 * H
			 * 
			 * process 3:
			 * J
			 */
			
			/*orders
			 * process 1:
			 * 	orders1:
			 * 	A
			 * 
			 * 	orders2:
			 * 	B,C,D(A:a:c)
			 * 
			 * 	orders3:
			 * 	E(D:d:e),F(A:a2:f, B:b:f2)<J,K
			 * 
			 * 	orders4:
			 * 	I(J:j:i)
			 * 
			 * process 2:
			 * 	orders1:
			 *	G
			 *
			 *	orders2:
			 * 	H
			 * 
			 * process 3:
			 * 	orders1:
			 * 	J
			 */
			
			/*waits = <
			 * process 1:
			 * 	orders3:
			 * 	(2)	J,K
			 * 
			 * process 2:
			 * 
			 * process 3:
			 */
			
			/*array = ,
			 * process 1:
			 * 	orders2:
			 * 	(3)	B,C,D(A:a:c)
			 * 
			 * 	orders3:
			 * 	(2)	E(D:d:e),F(A:a2:f, B:b:f2)<J,K
			 */
			
			/*order
			 * process 1:
			 * 	orders1:
			 * 		order1:
			 * 		A
			 * 
			 * 	orders2:
			 * 		order1:
			 * 		B
			 * 
			 * 		order2:
			 * 		C
			 * 
			 * 		order3:
			 * 		D(A:a:c)
			 * 
			 * 	orders3:
			 * 		order1:
			 * 		E(D:d:e)
			 * 
			 * 		order2:
			 * 		F(A:a2:f, B:b:f2)
			 * 
			 * 	orders4:
			 * 		order1:
			 *	 	I(J:j:i)
			 * 
			 * process 2:
			 * 	orders1:
			 *		order1:
			 *		G
			 *
			 *	orders2:
			 *		order1:
			 * 		H
			 */
			
			/*parameter = (:::)
			 * process 1:
			 * 	
			 * 	orders2:
			 * 		order3:
			 * 		D(A:a:c)
			 * 
			 * 	orders3:
			 * 		order1:
			 * 		E(D:d:e)
			 * 
			 * 		order2:
			 * 		F(A:a2:f, B:b:f2)
			 * 
			 * 	orders4:
			 * 		order1:
			 *	 	I(J:j:i)
			 */
			
			/*
			 * 階層構造的には、
			 * Processes / process / orders / waits,array / order / parameter
			 */
			
		val p = new MondogrossoProcessParser(input)
			
		
		"get finally-id in context" in {
			p.getFinally == "Z" must beTrue
		}
		
		"get total-num of processes in context" in {
			p.getProcessNum == 3 must beTrue
		}
		
		"get each-length of process in context" in {
			p.getEachProcessLength == List(5,2,1) must beTrue
		}
		
		
		
		//in process
		
		"get process of specific process" in {
			val dummyP = p.createProcess("dummy")
			val process = p.getProcess("process1").get
			/*
			 * A
			 * B,C,D(A:a:c)
			 * E(D:d:e),F(A:a2:f, B:b:f2)<J,K
			 * I(J:j:i)
			 */
			process.getEachOrdersLength == Seq(1,3,2,1) must beTrue
		}
		
		"get size of specific process" in {
			val process = p.getProcess("process1")
			process.size == 5 must beTrue
		}
		
		"get waits of specific process's specific orders" in {
			val process = p.getProcess("process1").get
			val orders = process.getOrdersAt(2)
			orders.getWaits == List("J","K") must beTrue
		}
		
		"get array-size of specific process's specific orders" in {
			val process = p.getProcess("process1").get
			val orders = process.getOrdersAt(1)
			orders.getArraySize == 3 must beTrue
		}
		
		//in order
		
		"get order that identified in specific process's specific orders " in {
			val process = p.getProcess("process1").get
			val orders = process.getOrdersAt(1)
			val order = orders.getOrder("A").get
			order.identity == "A" must beTrue
		}
		
		
	}
		
	
}