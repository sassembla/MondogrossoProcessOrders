package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap

import scala.sys.process._

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {
	
//	"orderInputs" should {
//		"have single triples" in {
//			val input = "(else:over:vie)"
//
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orderInputs, input).get
//			result.orderInputTriples.length == 1 must beTrue
//
//			val s = result.orderInputTriples(0)
//			result.orderInputTriples(0).identity.str == "else") must beTrue
//		}
//
//		"have multiple triples" in {
//			val input = "(else:over:vie,else2:over2:vie2)"
//
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orderInputs, input).get
//
//			result.orderInputTriples.length == 2 must beTrue
//
//			result.orderInputTriples(1).identity.str == "else2") must beTrue
//		}
//
//		"have multiple triples with whitespace" in {
//			val input = "(else:over:vie, else2:over2:vie2)"
//
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orderInputs, input).get
//
//			result.orderInputTriples.length == 2 must beTrue
//
//			result.orderInputTriples(1).identity.str == "else2") must beTrue
//		}
//	}
	
	"サンプル" should {
		"all	finally付きのフルセット	" in {
			val input = "A(else:over:vie else:over:vie)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!Z"
			
			val json = ""
			val parser = new MondogrossoProcessParser(input, json)
			val result = parser.parse
			/*
			 * All(
			 * 	Processes(
			 * 		List(
			 * 			Orders(List(Order(OrderIdentity(A),OrderInputs(List(OrderInputTriple(OrderString(else),OrderString(over),OrderString(vie)), OrderInputTriple(OrderString(else),OrderString(over),OrderString(vie)))),WaitOrders(List())))), 
			 * 			Orders(List(Order(OrderIdentity(AB),OrderInputs(List(OrderInputTriple(OrderString(elseB),OrderString(overB),OrderString(vieB)), OrderInputTriple(OrderString(elseB),OrderString(overB),OrderString(vieB)))),WaitOrders(List(OrderIdentity(SB)))))))),OrderIdentity(Z))
			 */
			
			//Ordersが2つあるはず
			result.processes.orders.size == 2 must beTrue
			
			//A,B,C,	AB,のOrderが入っているはず
			val the1stOrders = result.processes.orders(0)
			the1stOrders.orders.size == 3 must beTrue
			println("here-4")
				val candidateOfA = the1stOrders.orders(0)
				candidateOfA.identity.str == "A" must beTrue 
				println("here-3	"+the1stOrders.orders(1))
				val candidateOfB = the1stOrders.orders(1)
				println("here-2.5	"+candidateOfB)
				candidateOfB.identity.str == "B" must beTrue
				println("here-2")
				val candidateOfC = the1stOrders.orders(2)
				println("here-1")
				candidateOfC.identity.str == "C" must beTrue
			println("here")
			val the2ndOrders = result.processes.orders(1)
			println("here1")
			the2ndOrders.orders.size == 1 must beTrue
			println("here2")
				val candidateOfAB = the2ndOrders.orders(0)
				println("here3")
				candidateOfAB.identity.str == "AB" must beTrue
				println("here4")
		}
	}
	

//	"samples" should {
//		
//
//		"processes	複数のProcessフルセット	、finallyなし" in {
//			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB"
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.processes, input)
//
//			
//		}
//
//		"all	複数のProcessフルセット	" in {
//			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!F"
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//
//		}
//
//		"all	複数のWaitあり、なしのProcessフルセット	" in {
//			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//
//		}
//
//		"all	複数のWaitなしのProcessフルセット	" in {
//			val input = "A(else:over:vie,else:over:vie)+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//
//		}
//
//		"all	複数のWait、パラメータなしのProcessフルセット	" in {
//			val input = "A>B+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
//			val json = ""
//
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//
//		}
//
//		"orders	<のあとにIdentityが一つあるケース" in {
//
//			val input = "A(else:over:vie,else:over:vie)<S"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orders, input)
//			
//		}
//
//		"orders	<のあとにIdentityが一つあるケース2 パラメータなし" in {
//
//			val input = "A<S"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orders, input)
//			
//		}
//
//		"orders	<のあとにIdentityが一つあるケース3 パラメータなし、複数のOrder" in {
//
//			val input = "A>B<S"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.orders, input)
//			
//		}
//
//		"order	<が無いケース" in {
//
//			val input = "A(else:over:vie,else:over:vie)"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parseAll(parser.order, input)
//			
//		}
//
//		"all	<が無いケース" in {
//
//			val input = "A(else:over:vie,else:over:vie)!F"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//			
//		}
//
//		"all	改行入りのケース1 改行コード" in {
//			val input = "A(else:over:vie else:over:vie)<S\n+AB(elseB:overB:vieB elseB:overB:vieB)<SB\n+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//			
//		}
//
//		"all	改行入りのケース2 改行される文章" in {
//			val input =
//				"""A(else:over:vie,else:over:vie)<S
//+AB(elseB:overB:vieB elseB:overB:vieB)<SB
//+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"""
//
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//			
//			
//		}
//		
//		"単純なOrderの連続" in {
//			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
//			val json = ""
//			val parser = new MondogrossoProcessParser(input, json)
//			val result = parser.parse
//			
//		}
//	}
	
}