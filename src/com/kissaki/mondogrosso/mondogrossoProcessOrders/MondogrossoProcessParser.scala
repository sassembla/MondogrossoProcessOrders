package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._
import scala.util.parsing.combinator.RegexParsers

/*
 * AST
 * context
 * 	process
 * 		orders
 * 			order
 * 				keyValues
 * 					keyValue
 * 				promisses
 * 					promiss
 */
trait MondogrossoProcessOrdersAST

case class ProcessOrdersContext(identity:String,process:Process) extends MondogrossoProcessOrdersAST
case class Process(orders: List[Orders], finallyOrder: Order) extends MondogrossoProcessOrdersAST
case class Orders(ordersId: String, orders: List[Order]) extends MondogrossoProcessOrdersAST
case class Order(orderIdentity: String, keyValues: OrderKeyValues, waitForOrder: Order, inputPromisses: OrderInputPromissKeyValues) extends MondogrossoProcessOrdersAST
case class OrderKeyValues(keyValues: List[OrderKeyValue]) extends MondogrossoProcessOrdersAST
case class OrderKeyValue(key:String, value:String) extends MondogrossoProcessOrdersAST
case class OrderInputPromissKeyValues(promissesArray: List[OrderInputPromissKeyValue]) extends MondogrossoProcessOrdersAST
case class OrderInputPromissKeyValue(sourceIdentity: String, sourceKey: String, destinationKey: String) extends MondogrossoProcessOrdersAST

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(originalProcessesSource: String, json: String) extends RegexParsers {
  println("processesSource is " + originalProcessesSource + "	/json	" + json)

  val PREFIX_FINALLY = "!"
  val PREFIX_PROCESS_DELIM = "/"
  val PREFIX_ORDERS_DELIM = ">"

  //
  val contextId = "sample"
  var classDescription = "testasas"

  def getContextId: String = {
    contextId
  }

  /**
   * 吐き出すクラス記述の文字列。ここからコンパイル開始
   */
  def getContextClassDesctiption: String = {
    classDescription
  }

  def generateClassDescription(): String = {
    /*
		 * ジェネレート処理を行う。
		 * 上記までの情報から、importやstateをくみ上げる。
		 */

    classDescription
  }
}