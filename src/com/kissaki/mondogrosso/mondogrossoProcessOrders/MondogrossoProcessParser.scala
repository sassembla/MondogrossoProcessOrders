package com.kissaki.mondogrosso.mondogrossoProcessOrders

import scala.collection.mutable._
import scala.util.parsing.combinator.RegexParsers
import java.util.UUID
import net.liftweb.json._
import net.liftweb.json.JsonParser.{ parse ⇒ parseWithLiftJSONParser }
import net.liftweb.json.JsonAST.JObject

trait MondogrossoProcessOrdersAST

case class OrderString(myStr : String) extends MondogrossoProcessOrdersAST
case class OrderIdentity(myId : String) extends MondogrossoProcessOrdersAST

case class Order(myOrderIdentity : OrderIdentity, orderInputs : OrderInputs, waitIdentities : WaitOrders) extends MondogrossoProcessOrdersAST

case class OrderInputTriple(myInputIdentity : OrderString, fromKey : OrderString, toKey : OrderString) extends MondogrossoProcessOrdersAST
case class OrderInputs(myOrderInputTripleList : List[OrderInputTriple]) extends MondogrossoProcessOrdersAST

case class WaitOrders(myWaitOrdersList : List[OrderIdentity]) extends MondogrossoProcessOrdersAST

case class Orders(myOrderList : List[Order]) extends MondogrossoProcessOrdersAST
case class FirstOrders(first : Orders) extends MondogrossoProcessOrdersAST
case class SecondaryOrders(seconds : Orders) extends MondogrossoProcessOrdersAST

case class Processes(myOrdersList : List[Orders]) extends MondogrossoProcessOrdersAST

case class All(processes : Processes, finallyOrder : OrderIdentity) extends MondogrossoProcessOrdersAST

/*
 * AST
 * 	process
 * 		orders x n
 * 			order x n
 * 				identity
 * 				inputs
 * 					input x n
 *	 					sourceIdentity、sourceKey、destinationKey
 * 				waits
 * 					waitIdentity x n
 */

/**
 * パース結果を格納するcase class
 */
case class ContextSource(initialParam : Map[String, Map[String,String]], eventualParam : Map[String, Map[String,String]], current : Current, finallyOrder : String, totalOrderCount : Int, totalProcessNum : Int)
case class Current(processList : List[Process])
case class Process(identity : String, currentIndex : Int, orderIdentityList : List[String], orderAdditional : Map[String, OrderAddition])
case class OrderAddition(inputsList : List[InputRule], waitIdentitiesList : List[String])
case class InputRule(sourceOrderIdentity : String, from : String, to : String)

/**
 * プロセスのパーサ
 * 文字列入力を受けて、内容をパースする。
 */
class MondogrossoProcessParser(id : String, input : String, jsonSource : String) extends RegexParsers {

	//プレ処理、改行削除
	val preprocessedInput = input.replaceAll("\\n", "")

	/**
	 * パース関数
	 */
	def parseProcess = {
		//初期インデックス値
		val defaultIndex = 0;

		//パース
		val result = parseAll(all, preprocessedInput).get

		//パースデータのマッピング
		val processSource = result.processes.myOrdersList.map { currentOrders =>
			//プロセスのidentity(自動生成)
			val processIdentity = UUID.randomUUID.toString

			//オーダー順
			val orderIdentities = for (order <- currentOrders.myOrderList) yield order.myOrderIdentity.myId

			//オーダーの追加情報の準備
			val orderAdditionalList = currentOrders.myOrderList.map { currentOrder =>
				val identity = currentOrder.myOrderIdentity.myId

				val inputsList = for (inputTriple <- currentOrder.orderInputs.myOrderInputTripleList)
					yield InputRule(inputTriple.myInputIdentity.myStr, inputTriple.fromKey.myStr, inputTriple.toKey.myStr)

				val waitOrdersList = for (waitOrderIdentity <- currentOrder.waitIdentities.myWaitOrdersList)
					yield waitOrderIdentity.myId

				//一度マップ化
				Map(identity -> OrderAddition(inputsList, waitOrdersList))
			}

			//オーダーの追加情報を一つのMapに纏める
			val orderAddition = orderAdditionalList.reduceLeft {
				(a, b) => a ++ b
			}

			//プロセスの合成
			Process(processIdentity, defaultIndex, orderIdentities, orderAddition)
		}
		val current = Current(processSource)

		val totalOrderCountList = for (orders <- processSource) yield orders.orderIdentityList.length
		val totalOrderCount = totalOrderCountList.reduceLeft { (a, b) => a + b }
		val totalProcessNum = processSource.length

		//JSONのパース結果をマップにして整形したもの
		val initialParam = parseJSON(jsonSource)
		println("initialParam	" + initialParam)
		ContextSource(
			initialParam, //Orderの実行時に使用されるKV
			Map(), //Orderの実行結果によってたまっていくKV(初期値は空)
			current, //このContextに含まれるOrderのList
			result.finallyOrder.myId, //finallyのIdentity
			totalOrderCount, //オーダー数の合計
			totalProcessNum //全プロセス数の合計
			)
	}

	/**
	 * JSONのパース
	 *
	 * JSONの値は、１次のネストのみ可能なkey-value型に限る。
	 * (initialにのみある制限で、eventual時であればこの制約は突破できるかもしれない。おすすめはしないけど。)
	 */
	def parseJSON(jsonInput : String) :Map[String, Map[String,String]] = {
		println("jsonInput	" + jsonInput)

		val origin = parseWithLiftJSONParser(jsonInput)
		/*
		 * JObject(List(JField(A,JObject(List(JField(type,JString(sh)), JField(class,JString(AShell.sh)), JField(exec,JString(myExec)))))))
		 */

		//ここで、lift-jsonの型を消す。
		val initial = origin match {
			case some : JObject => {
				/*
				 * List(JField(A,JObject(List(JField(type,JString(sh)), JField(class,JString(AShell.sh)), JField(exec,JString(myExec))))))
				 */
				println("some	" + some)

				//まず再外郭のパラメータをList[Map[identity -> params]]で得る
				val orderMap = 
//					List(
//					Map("A" -> Map("type" -> "sh")),
//					Map("A" -> Map("class" -> "AShell.sh")),
//					Map("A" -> Map("exec" -> "myExec")),
//					Map("B" -> Map("class" -> "AShell.sh")))
									
					for {
					JObject(items) <- some
					item <- items
					JField(identity, keyValuesListObj) <- item
					JObject(keyValuesList) <- keyValuesListObj
					keyValue <- keyValuesList
					JField(key, valueString) <- keyValue
					JString(value) <- valueString
				} yield Map(identity -> Map(key->value))

				//オリジナリティのあるキーだけにする
				val keys = (for (map <- orderMap) yield map.keys.head).distinct
				
				val currentOrderIdentityInputList = for (key <- keys) yield { //キーごとに
					val allInputsByIdentity = orderMap.filter(_.keys.head.equals(key)).map { //List(Map(A -> Map(type -> sh)), Map(A -> Map(class -> AShell.sh)), Map(A -> Map(exec -> myExec)))
						case target : Map[String, Map[String, String]] => {
							val currentKey = target.keys.head
							println("target	" + target.get(currentKey).get)
							target.get(currentKey).get
						}
					}
					
					//ここで、keyごとにMap(key -> value),Map(key -> value),, になっているので、reduce
					val reduced = allInputsByIdentity.reduceLeft{
						(total, toAdd) => total ++ toAdd
					}
					
					//keyをつけて、reduceした値と纏める
					Map(key -> reduced)
				}
				
				//この時点でIdentityごとのMapになっているので、さらにreduceでまとめる
				val currentOrderIdentityInput = currentOrderIdentityInputList.reduceLeft {
					(total, toAdd) => total ++ toAdd
				}
				
//				println("finally	"+currentOrderIdentityInput)
				currentOrderIdentityInput
			}
			case other => {
				println("other(bust be error)	" + other)
				Map(UUID.randomUUID().toString -> Map(UUID.randomUUID().toString -> UUID.randomUUID().toString))
			}
		}
		initial
	}

	/**
	 * key-valueの文字列
	 */
	def str : Parser[OrderString] = """[^()\>\<=:\s\!\,]*""".r ^^ {
		case value => OrderString(value)
	}

	/**
	 * identity
	 */
	def identity : Parser[OrderIdentity] = """[^()\>\<=:\s\!\+\,]*""".r ^^ {
		case value => OrderIdentity(value)
	}

	/**
	 * InputTriple
	 * A:a:c
	 */
	def orderInputTriple : Parser[OrderInputTriple] = str ~ ":" ~ str ~ ":" ~ str ~ ("," | "") ^^ { default =>
		println("orderInputTriple	" + default)

		default match {
			//continue
			case ((((((key : OrderString) ~ ":") ~ (from : OrderString)) ~ ":") ~ (to : OrderString)) ~ ",") => {
				println("continue	" + default)
				OrderInputTriple(key, from, to)
			}

			//last or single
			case ((((((key : OrderString) ~ ":") ~ (from : OrderString)) ~ ":") ~ (to : OrderString)) ~ "") => {
				println("last or single	" + default)
				OrderInputTriple(key, from, to)
			}
		}
	}

	/**
	 * OrderInputs
	 * (A:a:c)
	 * (A:a:c,D:d:e,F:f:g)
	 */
	def orderInputs : Parser[OrderInputs] = "(" ~> rep(orderInputTriple) <~ ")" ^^ { default =>
		println("orderInputs	" + default)

		default match {
			case (orderInputTriples : List[OrderInputTriple]) => {
				println("orderInputTriples	" + orderInputTriples)
				OrderInputs(orderInputTriples)
			}
		}
	}

	/**
	 * waitIdentity
	 * <A
	 */
	def waitIdentity2nd : Parser[OrderIdentity] = ("," ~ identity) ^^ { default =>
		println("waitIdentity2nd	" + default)
		default match {
			case ("," ~ (id : OrderIdentity)) => {
				id
			}
		}
	}

	/**
	 * wait
	 * <A
	 * <A<B<C
	 */
	def waitOrdersOrNot : Parser[WaitOrders] = ("<" ~ identity ~ rep(waitIdentity2nd) | "") ^^ { default =>
		println("waitOrdersOrNot	" + default)
		default match {
			case (("<" ~ (the1st : OrderIdentity)) ~ (the2nd : List[OrderIdentity])) => {
				WaitOrders(List(the1st) ++ the2nd)
			}
			case _ => {
				WaitOrders(List())
			}
		}
	}

	/**
	 * Order
	 * A
	 * A<B
	 * A(A2:a2:a)<B
	 * A(A2:a2:a, B:b:c)<D
	 */
	def order : Parser[Order] = (identity ~ (orderInputs | "") ~ (waitOrdersOrNot | "")) ^^ { default =>
		println("order	" + default)

		default match {
			//フルセット
			case (((id : OrderIdentity)) ~ (inputs : OrderInputs) ~ (waitOrders : WaitOrders)) => {
				println("full case2 " + default)
				Order(id, inputs, waitOrders)
			}
			//idとwait
			case (((id : OrderIdentity)) ~ _ ~ (waitOrders : WaitOrders)) => {
				println("full case3 " + default)
				Order(id, OrderInputs(List()), waitOrders)
			}
			case _ => {
				println("それ以外	　パースエラーにしたい" + default)
				Order(null, null, null)
			}
		}
	}

	/**
	 * >で繋がれる2番目以降のOrder
	 */
	def secondaryOrder : Parser[Order] = (">" ~ order) ^^ { default =>
		default match {
			case (">" ~ (secondaryOrder : Order)) => {
				secondaryOrder
			}
		}
	}

	/**
	 * オーダーの集合
	 */
	def orders : Parser[Orders] = order ~ (rep(secondaryOrder) | "") ^^ { default =>
		println("orders	" + default)

		default match {
			//複数
			case ((firstOrder : Order) ~ (seconds : List[Order])) => {
				Orders(List(firstOrder) ++ seconds)
			}

			//一つ
			case ((singleOrder : Order)) => {
				Orders(List(singleOrder))
			}
		}
	}

	/**
	 * 二つ目以降のOrders
	 * parallelに実行される
	 */
	def secondaryOrders : Parser[SecondaryOrders] = ("+" ~ (orders)) ^^ { default =>
		println("secondaryOrders	" + default)
		default match {
			case "+" ~ (secondalyOrder : Orders) => {
				SecondaryOrders(secondalyOrder)
			}
		}
	}

	/**
	 * オーダーの集合Ordersの集合
	 */
	def processes : Parser[Processes] = orders ~ (rep(secondaryOrders) | "") ^^ { default =>

		println("processes	" + default)

		default match {

			case ((firstOrders : Orders) ~ (secondaryOrdersList : List[SecondaryOrders])) => {
				println("firstOrders	" + firstOrders + "	/secondaryOrdersList	" + secondaryOrdersList)

				//やりたいのは、secondaryOrdersList中のordersの中のsecondsを取り出してリストにすること
				val s = for (a <- secondaryOrdersList) yield a.seconds

				//リストにして結合
				val appended = List(firstOrders) ++ s
				Processes(appended)
			}
			case _ => {
				println("一個のOrdersも解釈できないエラー	" + default)
				//空のリストを返す
				Processes(List())
			}
		}
	}

	/**
	 * finallyも含めた全体
	 */
	def all : Parser[All] = processes ~ "!" ~ identity ^^ { default =>
		println("all	" + default)

		default match {
			case (all ~ _ ~ finallyOrderId) => {
				println("all = " + all)
				println("final = " + finallyOrderId)
				val result = All(all, finallyOrderId)
				result
			}
		}
	}
}