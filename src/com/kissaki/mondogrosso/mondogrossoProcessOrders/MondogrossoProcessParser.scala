package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._
import scala.util.parsing.combinator.RegexParsers

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
class MondogrossoProcessParser(originalProcessesSource : String, json : String) {
	println("processesSource is " + originalProcessesSource + "	/json	" + json)

	val parser = new CopiedParser
	//頭の+>は無ければつける、とかかなあ、、

	val result = parser.parseAll(parser.process, originalProcessesSource)

	println("result	" + result.get)

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
}

trait MondogrossoProcessOrdersAST
case class OrderString(str : String) extends MondogrossoProcessOrdersAST
case class OrderIdentity(str : String) extends MondogrossoProcessOrdersAST

case class Order(identity : OrderIdentity, orderInputs : OrderInputs, waitIdentities : WaitOrders) extends MondogrossoProcessOrdersAST

case class OrderInputTriple(identity : OrderString, fromKey : OrderString, toKey : OrderString) extends MondogrossoProcessOrdersAST
case class OrderInputs(orderInputTriples : List[OrderInputTriple]) extends MondogrossoProcessOrdersAST

case class WaitOrders(waitOrdes:List[OrderIdentity]) extends MondogrossoProcessOrdersAST

case class Orders(orders : List[Order]) extends MondogrossoProcessOrdersAST
case class Processes(orders : Orders) extends MondogrossoProcessOrdersAST

case class All(processes : Processes, finallyOrder : OrderIdentity) extends MondogrossoProcessOrdersAST

case class ASTProperty(key : OrderString, value : OrderString) extends MondogrossoProcessOrdersAST

case class ASTSection(section : Order, inputs : List[OrderInputs]) extends MondogrossoProcessOrdersAST

case class ASTSections(sections : List[Order]) extends MondogrossoProcessOrdersAST

class CopiedParser extends RegexParsers {
	/**
	 * key-valueの文字列
	 */
	def str : Parser[OrderString] = """[^()\>\<=:\s\!]*""".r ^^ {
		case value => OrderString(value)
	}

	/**
	 * identity
	 */
	def identity : Parser[OrderIdentity] = """[^()\>\<=:\s\!]*""".r ^^ {
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
	 * 一つだけのwaitに対応
	 */
	def waitOrders : Parser[WaitOrders] = ("<"~identity | "") ^^ {
		case a:String => {
			WaitOrders(List())
		}
			case _~waitId => {
				waitId match {
					case currentWait:OrderIdentity => {
						WaitOrders(List(currentWait))
					}
				}
			}
			case _ => {
				WaitOrders(List())
			}
	}

	/**
	 * Order
	 * >A(A:a:b)<C
	 */
	def order : Parser[Order] = ">" ~> identity ~ orderInputs ~ waitOrders ^^ {
		case (id ~ inputs ~ c) => {
			println("idIs	" + id + "	/inputs	" + inputs + "	/" + c)
			Order(id, inputs, c)
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
	def process : Parser[Processes] = orders ~ ("" | rep("+" ~ orders)) ^^ {
		case (theOrders ~ other) => {
			
			println("theOrders	"+theOrders)
			println("other	"+other)
			
			val result = Processes(theOrders)
			println("processend	"+result)

			result
		}
	}
	
	/**
	 * finallyも含めた全体
	 */
	def all : Parser[All] = process ~ "!" ~ identity ^^ {
		case (all ~ _ ~ finallyOrderId) => {
			println("all = " + all)
			println("final = " + finallyOrderId)
			val result = All(all, finallyOrderId)
			result
		}

	}
}