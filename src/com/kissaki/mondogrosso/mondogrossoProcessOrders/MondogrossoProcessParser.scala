package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._
import scala.util.parsing.combinator.RegexParsers

trait MondogrossoProcessOrdersAST
case class OrderString(str : String) extends MondogrossoProcessOrdersAST
case class OrderIdentity(str : String) extends MondogrossoProcessOrdersAST

case class Order(identity : OrderIdentity, orderInputs : OrderInputs, waitIdentities : WaitOrders) extends MondogrossoProcessOrdersAST

case class OrderInputTriple(identity : OrderString, fromKey : OrderString, toKey : OrderString) extends MondogrossoProcessOrdersAST
case class OrderInputs(orderInputTriples : List[OrderInputTriple]) extends MondogrossoProcessOrdersAST

case class WaitOrders(waitOrders : List[OrderIdentity]) extends MondogrossoProcessOrdersAST

case class Orders(orders : List[Order]) extends MondogrossoProcessOrdersAST
case class Processes(orders : Orders) extends MondogrossoProcessOrdersAST

case class All(processes : Processes, finallyOrder : OrderIdentity) extends MondogrossoProcessOrdersAST

case class ASTProperty(key : OrderString, value : OrderString) extends MondogrossoProcessOrdersAST

case class ASTSection(section : Order, inputs : List[OrderInputs]) extends MondogrossoProcessOrdersAST

case class ASTSections(sections : List[Order]) extends MondogrossoProcessOrdersAST

/*
 * AST
 * context
 * 	process
 * 		orders
 * 			order
 * 				identity
 * 				inputs
 * 					input
 *	 					sourceIdentity、sourceKey、destinationKey
 * 				defaultKeyValues
 * 					defaultKeyValue
 * 				waitIdentities
 * 					waitIdentity
 */

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(input : String) extends RegexParsers {

	//プレ処理、改行削除
	val preprocessedInput = input.replaceAll("\\n", "")
	println("\n preprocessedInput	" + preprocessedInput)

	/**
	 * パース関数
	 */
	def parse = {
		parseAll(all, preprocessedInput)
	}

	//ここでパースが終わっている
	val contextId = "sample"
	var classDescription = "testasas"

	def getContextId : String = {
		contextId
	}

	/**
	 * 吐き出すクラス記述の文字列。ここからコンパイル開始
	 */
	def getContextClassDesctiption : String = {
		classDescription
	}

	def generateClassDescription() : String = {
		/*
		 * ジェネレート処理を行う。
		 * 上記までの情報から、importやstateをくみ上げる。
		 */

		classDescription
	}

	/**
	 * key-valueの文字列
	 */
	def str : Parser[OrderString] = """[^()\>\<=:\s\!]*""".r ^^ {
		case value => OrderString(value)
	}

	/**
	 * identity
	 */
	def identity : Parser[OrderIdentity] = """[^()\>\<=:\s\!\+]*""".r ^^ {
		case value => OrderIdentity(value)
	}

	/**
	 * InputTriple
	 * A:a:c
	 */
	def orderInputTriple : Parser[OrderInputTriple] = str ~ ":" ~ str ~ ":" ~ str ^^ {
		case (key ~ _ ~ from ~ _ ~ to) => OrderInputTriple(key, from, to)
	}

	/**
	 * OrderInputs
	 * (A:a:c D:d:e F:f:g)
	 */
	def orderInputs : Parser[OrderInputs] = "(" ~> rep(orderInputTriple) <~ ")" ^^ {
		case (orderInputTriples) => {
			println("orderInputTriples	" + orderInputTriples)
			OrderInputs(orderInputTriples)
		}
	}
	
	/**
	 * wait(一つのみ)
	 */
	def waitOrdersOrNot : Parser[WaitOrders] = (("<" ~ identity) | "") ^^ {org =>
		println("org	"+org)
		org match {
			case ("<"~(s:OrderIdentity)) => {
				println("wait on	"+s)
				WaitOrders(List(s))
			}
			case _ => {
				println("wait void	"+org)
				WaitOrders(List())
			}
		}
	}

	/**
	 * Order
	 * >A(A:a:b)<C,
	 * >A<C,
	 * 最初を空白にしたいんだけどなー。
	 */
	def order : Parser[Order] = ((">" ~ identity) ~ (orderInputs | "") ~ (waitOrdersOrNot | "")) ^^ { orderOrigin =>
		println("orderOrigin	" + orderOrigin)
		
		orderOrigin match {
			//フルセット
			case ((">"~(id:OrderIdentity))~(inputs:OrderInputs)~(waitOrders:WaitOrders)) => {
				print("full case2 "+orderOrigin)
				Order(id, inputs, waitOrders)
			}
			//idとwait
			case ((">"~(id:OrderIdentity))~_~(waitOrders:WaitOrders)) => {
				print("full case3 "+orderOrigin)
				Order(id, null, waitOrders)
			}
			
			//始まり
			
			//フルセット
			case (((id:OrderIdentity))~(inputs:OrderInputs)~(waitOrders:WaitOrders)) => {
				print("full case2 "+orderOrigin)
				Order(id, inputs, waitOrders)
			}
			//idとwait
			case (((id:OrderIdentity))~_~(waitOrders:WaitOrders)) => {
				print("full case3 "+orderOrigin)
				Order(id, null, waitOrders)
			}
			case _ => {
				println("それ以外	　パースエラーにしたい"+orderOrigin)
				Order(null, null, null)
			}
		}
	}

	/**
	 * オーダーの集合
	 */
	def orders : Parser[Orders] = rep(order) ^^ {
		case (orders) => {
			println("orders	" + orders)
			Orders(orders)
		}
	}

	/**
	 * オーダーの集合の集合
	 */
	def processes : Parser[Processes] = orders ~ (rep("+" ~ orders) | "") ^^ { something =>

		println("processes something	" + something)

		something match {
			case (first ~ "") => {

				println("first	" + first)

				val result = Processes(first)
				println("processend	" + result)

				result
			}
			case _ => {
				//空のリストを返す
				Processes(Orders(List()))
			}

			//		case (first ~ _ ~ rest) => {
			//			
			//			println("rest	"+rest)
			//			Processes(first)
			//		}
		}
	}

	/**
	 * finallyも含めた全体
	 */
	def all : Parser[All] = processes ~ "!" ~ identity ^^ {
		case (all ~ _ ~ finallyOrderId) => {
			println("all = " + all)
			println("final = " + finallyOrderId)
			val result = All(all, finallyOrderId)
			result
		}
	}
}