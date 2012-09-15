package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap

import scala.sys.process._

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {
	trait cont extends BeforeAfter {
		def before = {
			println("before")
		}

		def after = {
			println("after")
		}
	}
	"samples" should {
		"all	finally付きのフルセット	" in {
			val input = ">A(else:over:vie else:over:vie)>B>C(a:v:s)<S+>AB(elseB:overB:vieB elseB:overB:vieB)<SB!Z"
			
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse

			println("result	" + result.get)
		}

		"processes	複数のProcessフルセット	、finallyなし" in {	
			val input = ">A(else:over:vie else:over:vie)<S+>AB(elseB:overB:vieB elseB:overB:vieB)<SB"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parseAll(parser.processes, input)

			println("result	" + result.get)
		}
		
		"all	複数のProcessフルセット	" in {	
			val input = ">A(else:over:vie else:over:vie)<S+>AB(elseB:overB:vieB elseB:overB:vieB)<SB!F"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse

			println("result	" + result.get)
		}
		
		"all	複数のWaitあり、なしのProcessフルセット	" in {	
			val input = ">A(else:over:vie else:over:vie)<S+>AB(elseB:overB:vieB elseB:overB:vieB)!F"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse

			println("result	" + result.get)
		}

		"all	複数のWaitなしのProcessフルセット	" in {	
			val input = ">A(else:over:vie else:over:vie)+>AB(elseB:overB:vieB elseB:overB:vieB)!F"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse

			println("result	" + result.get)
		}
		
		"all	複数のWait、パラメータなしのProcessフルセット	" in {	
			val input = ">A>B+>AB(elseB:overB:vieB elseB:overB:vieB)!F"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse

			println("result	" + result.get)
		}

		"orders	<のあとにIdentityが一つあるケース" in {
			
			val input = ">A(else:over:vie else:over:vie)<S"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parseAll(parser.orders, input)
			println("result	" + result.get)
		}
		
		"orders	<のあとにIdentityが一つあるケース2 パラメータなし" in {
			
			val input = ">A<S"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parseAll(parser.orders, input)
			println("result	" + result.get)
		}
		
		"orders	<のあとにIdentityが一つあるケース3 パラメータなし、複数のOrder" in {
			
			val input = ">A>B<S"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parseAll(parser.orders, input)
			println("result	" + result.get)
		}
	
		"order	<が無いケース" in {
			
			val input = ">A(else:over:vie else:over:vie)"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parseAll(parser.order, input)
			println("result	" + result.get)
		}
		
		"all	<が無いケース" in {
			
			val input = ">A(else:over:vie else:over:vie)"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse
			println("result	" + result.get)
		}
		
		
		"all	改行入りのケース1 改行コード" in {
			val input = ">A(else:over:vie else:over:vie)<S\n+>AB(elseB:overB:vieB elseB:overB:vieB)<SB\n+>AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse
			println("result	" + result.get)
		}
		
		"all	改行入りのケース2 改行される文章" in {
			val input = 
				""">A(else:over:vie else:over:vie)<S
+>AB(elseB:overB:vieB elseB:overB:vieB)<SB
+>AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"""
				
			val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
			val parser = new MondogrossoProcessParser(input)
			val result = parser.parse
			println("result	" + result.get)
		}
		

		/*
		 * +
+>A2>B2(else:over:vie else:over:vie2)

		 */

	}
	//	"parseOrder parse" should {
	//		//+,.が使えない、、、マジか、、、　文字列に対するimplicitがあるな。
	//		val input = "A>B,C,D(A:a:c)>E(D:d:e),F(A:a2:f, B:b:f2)<J,K>I(J:j:i)/D>G>H/G>J!Z"
	//		val json = "{\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}},\"Z\": {\"type\": \"process\",\"class\": \"Z\",\"exec\": \"exec\",\"kv\": {\"key1\": \"value1\",\"key2\": \"value2\"}}}}}"
	//
	//		val p = new MondogrossoProcessParser(input, json)
	//
	//		/*ORDER A
	//			 * {
	//			 * 	"A": {
	//			 * 		"type": "process",
	//			 * 		"class": "A",
	//			 * 		"exec": "exec",
	//			 * 		"kv": {
	//			 * 			"key1": "value1",
	//			 * 			"key2": "value2"
	//			 * 		}
	//			 * 	}
	//			 * }
	//			 */
	//
	//		/*ORDER Z
	//			 * {
	//			 * 	"Z": {
	//			 * 		"type": "process",
	//			 * 		"class": "Z",
	//			 * 		"exec": "exec",
	//			 * 		"kv": {
	//			 * 			"key1": "value1",
	//			 * 			"key2": "value2"
	//			 * 		}
	//			 * 	}
	//			 * }
	//			 */
	//
	//		/*processes =
	//			 * A>B,C,D(A:a:c)>E(D:d:e),F(A:a2:f, B:b:f2)<J,K>I(J:j:i)
	//			 *  .D>G>H
	//			 *  .G>J
	//			 */
	//
	//		/*
	//			 * 階層構造的には、
	//			 * Processes / process / orders - waits,array / order - parameter
	//			 */
	//
	//		/*finally = !
	//			 * !Z
	//			 */
	//
	//		/*process = >
	//			 * process 1:
	//			 * A
	//			 * B,C,D(A:a:c)
	//			 * E(D:d:e),F(A:a2:f, B:b:f2)<J,K
	//			 * I(J:j:i)
	//			 * 
	//			 * process 2:
	//			 * G
	//			 * H
	//			 * 
	//			 * process 3:
	//			 * J
	//			 */
	//
	//		/*orders
	//			 * process 1:
	//			 * 	orders1:
	//			 * 	A
	//			 * 
	//			 * 	orders2:
	//			 * 	B,C,D(A:a:c)
	//			 * 
	//			 * 	orders3:
	//			 * 	E(D:d:e),F(A:a2:f, B:b:f2)<J,K
	//			 * 
	//			 * 	orders4:
	//			 * 	I(J:j:i)
	//			 * 
	//			 * process 2:
	//			 * 	orders1:
	//			 *	G
	//			 *
	//			 *	orders2:
	//			 * 	H
	//			 * 
	//			 * process 3:
	//			 * 	orders1:
	//			 * 	J
	//			 */
	//
	//		/*waits = <
	//			 * process 1:
	//			 * 	orders3:
	//			 * 	(2)	J,K
	//			 * 
	//			 * process 2:
	//			 * 
	//			 * process 3:
	//			 */
	//
	//		/*array = ,
	//			 * process 1:
	//			 * 	orders2:
	//			 * 	(3)	B,C,D(A:a:c)
	//			 * 
	//			 * 	orders3:
	//			 * 	(2)	E(D:d:e),F(A:a2:f, B:b:f2)<J,K
	//			 */
	//
	//		/*order
	//			 * process 1:
	//			 * 	orders1:
	//			 * 		order1:
	//			 * 		A
	//			 * 
	//			 * 	orders2:
	//			 * 		order1:
	//			 * 		B
	//			 * 
	//			 * 		order2:
	//			 * 		C
	//			 * 
	//			 * 		order3:
	//			 * 		D(A:a:c)
	//			 * 
	//			 * 	orders3:
	//			 * 		order1:
	//			 * 		E(D:d:e)
	//			 * 
	//			 * 		order2:
	//			 * 		F(A:a2:f, B:b:f2)
	//			 * 
	//			 * 	orders4:
	//			 * 		order1:
	//			 *	 	I(J:j:i)
	//			 * 
	//			 * process 2:
	//			 * 	orders1:
	//			 *		order1:
	//			 *		G
	//			 *
	//			 *	orders2:
	//			 *		order1:
	//			 * 		H
	//			 */
	//
	//		/*parameter = (:::)
	//			 * process 1:
	//			 * 	
	//			 * 	orders2:
	//			 * 		order3:
	//			 * 		D(A:a:c)
	//			 * 
	//			 * 	orders3:
	//			 * 		order1:
	//			 * 		E(D:d:e)
	//			 * 
	//			 * 		order2:
	//			 * 		F(A:a2:f, B:b:f2)
	//			 * 
	//			 * 	orders4:
	//			 * 		order1:
	//			 *	 	I(J:j:i)
	//			 */
	//
	//		"have context" in new cont {
	//			val contextId = p.getContextId
	//			contextId != None must beTrue
	//		}
	//
	//		"context will be create class:MondogrossoCurrentOrder & MondogrossoCurrentOrderRunner-instance" in {
	//			val contextClassDesc = p.getContextClassDesctiption
	//			contextClassDesc != None
	//		}
	//
	//		"get finally-id in context" in {
	//			//			p.getFinally == "Z" must beTrue
	//		}
	//
	//		"get total-num of processes in context" in {
	//			//			p.getProcessNum == 3 must beTrue
	//		}
	//
	//		"get each-length of process in context" in {
	//			//			p.getEachProcessLength == List(5,2,1) must beTrue
	//		}
	//
	//		//inside process
	//
	//		//		"get process of specific process" in {
	//		//			val dummyP = p.createProcess("dummy", "dummy2")
	//		//			val process = p.getProcess("process1").get
	//		//			/*
	//		//			 * A
	//		//			 * B,C,D(A:a:c)
	//		//			 * E(D:d:e),F(A:a2:f, B:b:f2)<J,K
	//		//			 * I(J:j:i)
	//		//			 */
	//		//			process.getEachOrdersLength == Seq(1,3,2,1) must beTrue
	//		//		}
	//		//		
	//		//		"get size of specific process" in {
	//		//			val process = p.getProcess("process1")
	//		//			/*
	//		//			 * A
	//		//			 * B,C,D(A:a:c)
	//		//			 * E(D:d:e),F(A:a2:f, B:b:f2)<J,K
	//		//			 * I(J:j:i)
	//		//			 */
	//		//			process.size == 4 must beTrue
	//		//		}
	//		//		
	//		//		"get waits of specific process's specific orders" in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrdersAt(2)
	//		//			orders.getWaits == List("J","K") must beTrue
	//		//		}
	//		//		
	//		//		"get array-size of specific process's specific orders" in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrdersAt(1)
	//		//			orders.getArraySize == 3 must beTrue
	//		//		}
	//		//		
	//		//		"get orders that identified by name of specific process" in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrders("order2").get
	//		//			orders.getArraySize == 3 must beTrue
	//		//		}
	//		//		
	//		//		"get order from index of specific process's specific orders" in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrdersAt(1)
	//		//			val order = orders.getOrderAt(1).get
	//		//			order.identity == "A" must beTrue
	//		//		}
	//		//		
	//		//		"get order that identified by name of specific process's specific orders " in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrdersAt(1)
	//		//			val order = orders.getOrder("A").get
	//		//			order.identity == "A" must beTrue
	//		//		}
	//		//		
	//		//		//inside order
	//		//		
	//		//		"get order's keys and values of params-information" in {
	//		//			val process = p.getProcess("process1").get
	//		//			val orders = process.getOrdersAt(1)
	//		//			val order = orders.getOrder("A").get
	//		//			
	//		//			order.identity == "A" must beTrue
	//		//			order.getAllParamKeysAndValues == ListMap("key1"->"value1","key2"->"value2") must beTrue
	//		//		}
	//	}
	//
	//	"processOrder-parsed has information for drive" should {
	//		val input = "A>B!Z"
	//		val json =
	//			"{" +
	//				"\"A\":{\"type\":\"process\",\"class\":\"A\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}}," +
	//				"\"B\":{\"type\":\"process\",\"class\":\"B\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}}," +
	//				"\"Z\":{\"type\":\"process\",\"class\":\"Z\",\"exec\":\"exec\",\"kv\":{\"key1\":\"value1\",\"key2\":\"value2\"}}" +
	//				"}"
	//
	//		/*
	//		 * パラメータを絞って試す
	//		 */
	//		val p = new MondogrossoProcessParser(input, json)
	//
	//		val contexifiedFileName = p.contextId + ".scala"
	//		val geneatedClassDesc = p.generateClassDescription
	//
	//		"generated class have imports" in {
	//
	//			//importが3つあるはず
	//			failure("not yet implemented, but will have 3-import")
	//
	//			//それぞれの内容がA,B,Zなハズ			
	//			failure("not yet implemented, but is A,B,Z")
	//
	//			val a = ""
	//			a != "" must beTrue
	//		}
	//
	//		//		"force generate Class-description then compile scala-class from current context" in {
	//		//			import org.apache.commons.io.FileUtils
	//		//			
	//		//			//書き出し
	//		//			Process("echo "+geneatedClassDesc) #> new java.io.File(contexifiedFileName) run
	//		//
	//		//			//全文を表示してみる
	//		//			scala.io.Source.fromFile("test.scala", "UTF-8").getLines.foreach{ println _ }
	//		//
	//		//			//コンパイルしてみる
	//		//			val s = Process("scala "+contexifiedFileName) run
	//		//			
	//		//			println("s	"+s)
	//		//			
	//		//			//コンパイルに成功してればclassが出来てるはず
	//		//			val klass = scala.io.Source.fromFile("test.class", "UTF-8")
	//		//			
	//		//			println("klass	"+klass)
	//		//			
	//		//			//全行表示
	//		//			scala.io.Source.fromFile("test.class", "UTF-8").getLines.foreach{ println _ }
	//		//			
	//		//			val a = ""
	//		//			a != "" must beTrue
	//		//		}
	//	}

}