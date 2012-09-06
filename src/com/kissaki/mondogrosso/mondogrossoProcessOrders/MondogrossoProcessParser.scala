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
	val result = parser.parseAll(parser.orders, """
[db1
			
[db2(else:over:vie else:over:vie)

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
		case (_~orderInputTriples~_) => {
			println("orderInputTriples	"+orderInputTriples)
			OrderInputs(orderInputTriples)
		}
	}

	/**
	 * Order
	 * A(A:a:b)
	 * 
	 * いまは、
	 * A(A:a:b)(D:d:e) 
	 * も赦している。repはずすのが難しい。
	 */
	def order : Parser[Order] = "[" ~> identity ~ rep(orderInputs) ^^ {
		case (id ~ inputs) => {
			println("id	"+id)
			
			val s = Order(id, inputs)
			println("s	"+s)
			s
		}
	}

	def orders : Parser[ASTSections] = rep(order) ^^ {
		case sections => {
			println("start")
			val result = ASTSections(sections)
			println("end")
			
			result
		}
	}
}