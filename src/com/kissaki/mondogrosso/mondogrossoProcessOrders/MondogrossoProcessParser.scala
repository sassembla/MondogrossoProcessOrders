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
//trait MondogrossoProcessOrdersAST

//case class ProcessOrdersContext(identity : String, process : Process) extends MondogrossoProcessOrdersAST
//case class Process(orders : List[Orders], finallyOrder : Order) extends MondogrossoProcessOrdersAST
//case class Orders(ordersId : String, orders : List[Order]) extends MondogrossoProcessOrdersAST
//
//case class Order(
//	orderIdentity : OrderIdentity,
//	keyValues : OrderDefaultKeyValues,
//	waitForOrders : OrderWaitForOrders,
//	inputs : OrderInputs) extends MondogrossoProcessOrdersAST
//
//case class OrderIdentity(identity : String) extends MondogrossoProcessOrdersAST
//
//case class OrderInputs(inputs : List[OrderInput]) extends MondogrossoProcessOrdersAST
//case class OrderInput(sourceIdentity : OrderString, sourceKey : OrderString, destinationKey : OrderString) extends MondogrossoProcessOrdersAST
//
//case class OrderWaitForOrders(waitForOrderIdentities : List[OrderIdentity]) extends MondogrossoProcessOrdersAST
//
//case class OrderDefaultKeyValues(keyValues : List[OrderDefaultKeyValue]) extends MondogrossoProcessOrdersAST
//case class OrderDefaultKeyValue(key : OrderString, value : OrderString) extends MondogrossoProcessOrdersAST
//
//case class OrderString(string : String) extends MondogrossoProcessOrdersAST

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(originalProcessesSource : String, json : String) {
	println("processesSource is " + originalProcessesSource + "	/json	" + json)

	val parser = new CopiedParser
	val result = parser.parseAll(parser.orders, """
[db1
			
[db2(else:over:vie else:over:vie)
(some:thing:di)
""")

	println("result	" + result.get)

	//	val parser2 = new Parser
	//	val result2 = parser2.parseAll(parser2.property, originalProcessesSource);
	//
	//	println("result2	" + result2.get)

	//
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

case class Order(identity : OrderIdentity, orderinputs : List[OrderInputs]) extends MondogrossoProcessOrdersAST

case class OrderInputTriple(identity : OrderString, fromKey : OrderString, toKey : OrderString)  extends MondogrossoProcessOrdersAST
case class OrderInputs(orderInputTriples:List[OrderInputTriple]) extends MondogrossoProcessOrdersAST

case class ASTProperty(key : OrderString, value : OrderString) extends MondogrossoProcessOrdersAST

case class ASTSection(section : Order, inputs : List[OrderInputs]) extends MondogrossoProcessOrdersAST

case class ASTSections(sections : List[Order]) extends MondogrossoProcessOrdersAST

class CopiedParser extends RegexParsers {
	/**
	 * key-valueの文字列
	 */
	def str : Parser[OrderString] = """[^()\[\]=:\s]*""".r ^^ {
		case value => OrderString(value)
	}

	/**
	 * identity
	 */
	def identity : Parser[OrderIdentity] = """[^()\[\]=:\s]*""".r ^^ {
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
	def orderInputs : Parser[OrderInputs] = "("~ rep(orderInputTriple) ~")" ^^ {
		case (_~orderInputTriples~_) => OrderInputs(orderInputTriples)
	}

	/**
	 * Order
	 * A(A:a:b)
	 */
	def order : Parser[Order] = "[" ~> identity ~ rep(orderInputs) ^^ {
		case (id ~ inputs) => Order(id, inputs)
	}

	def orders : Parser[ASTSections] = rep(order) ^^ {
		case sections => ASTSections(sections)
	}
}