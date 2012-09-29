package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID
import com.kissaki.TagValue

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersControllerTests extends Specification {

	val standardJSON = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo",
								"a":"something",
								"a2":"else",
								"a3":"other",
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "echo",
								"c":"ready"
							},
						"D": 
							{
								"_kind": "sh",
								"_main": "echo",
								"d1":"beforeD1",
								"d2":"beforeD2"
		
							},
						"E": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__delay":"10000"
							}
						}"""

	//OrderController	
	if (false) {
		"OrderController" should {

			"Contextを実行開始" in {
				val orderCont = new MondogrossoProcessOrdersController
				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
				val json = standardJSON

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = orderCont.attachProcess(identity, result)

				//contextを生成
				val currentContext = orderCont.contexts(0)
				//現在実行中のOrder、内容がまだ無い
				orderCont.contexts(0).currentExecutingOrders.length must be_==(0)

				//起動
				orderCont.runAllContext

				//実行開始したことによって、Messagingが発生、最初のProcessがStartし、現在のindexが0になっているはず
				orderCont.contexts(0).currentOrderIndex must be_==(0)

				//内容が,A
				orderCont.contexts(0).currentExecutingOrders(0) must be_==("A")
			}
		}
	}

	//Context information
	if (false) {
		"Context information" should {
			val id = UUID.randomUUID().toString
			val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parseProcess

			
			"Contextを生成した時点で、Context内での値はすべてSequenceとして存在しているはず" in {
				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//プロセス数を取得(=で将来作られるWorker数)
				currentContext.processNum must be_==(2)

				//全体インデックスを取得
				currentContext.totalOrderNum must be_==(6)

				//現在の進捗インデックスを取得
				currentContext.currentOrderIndex must be_==(0)
			}

			"最初から実行	現在実行中のOrderがAなハズ" in {
				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)
				//			currentContext.run()
				//			println("currentContext.currentExecutingOrders(0)	" + currentContext.currentExecutingOrders(0))
				//			currentContext.currentExecutingOrders(0) must be_==("A")
				"not yet applied" must be_==("")
			}

			"途中のindexから実行" in {
				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)
				//			currentContext.startFrom(2)
				//			
				//			//現在実行中のOrderがAなハズ
				//			currentContext.currentExecutingOrders must be_==(Map("A"))

				"not yet applied" must be_==("")
			}
		}
	}

	//Context
	if (true) {
		"Context run" should {
//			"手順的に一つずつの手順ログを持つはず" in {
//				"not yet applied" must be_==("")
//			}
//
//			"before run" in {
//				
//				val id = UUID.randomUUID().toString
//				val input = "A!Z"
//				val json = """
//						{"A": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l"
//							},
//						"Z": 
//							{
//								"_kind": "sh",
//								"_main": "pwd",
//								"__finallyTimeout":"10000"
//							}
//						}
//						"""
//
//				val parser = new MondogrossoProcessParser(id, input, json)
//				val result = parser.parseProcess
//
//				val identity = UUID.randomUUID().toString
//				val currentContext = new ProcessContext(identity, result)
//
//				//run前、ContextのstatusがREADY
//				currentContext.currentStatus must be_==(ContextStatus.STATUS_READY)
//			}

			"run A then Z" in {
				val id = UUID.randomUUID().toString
				val input = "A!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext
				
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想	"+currentContext.currentStatus)
				val waiter = new DummyParent()
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想1	"+currentContext.currentStatus)
				waiter.waitTime(100)
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想2	"+currentContext.currentStatus)
				
				currentContext.currentStatus must be_==(ContextStatus.STATUS_DONE)
				
				//Aの実行記録がある
				currentContext.currentContext.get("A").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				//Zの実行記録がある
				currentContext.currentContext.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))
					
				//最終的なContextに特定の値が含まれる
				println("currentContext.currentContext	A to Z	"+currentContext.currentContext)
			}
			
			"run A then Z with finallyTimeout" in {
				val id = UUID.randomUUID().toString
				val input = "A!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext
				
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想	"+currentContext.currentStatus)
				val waiter = new DummyParent()
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想1	"+currentContext.currentStatus)
				waiter.waitTime(100)
				println("非同期のプロセス一つを必ず挟むので、100くらい待つのが理想2	"+currentContext.currentStatus)
				
				currentContext.currentStatus must be_==(ContextStatus.STATUS_DONE)
				
				//Aの実行記録がある
				currentContext.currentContext.get("A").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				//Zの実行記録がある
				currentContext.currentContext.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__finallyTimeout.toString,
					OrderPrefix._result.toString))
					
				//最終的なContextに特定の値が含まれる
				println("currentContext.currentContext	A to Z	"+currentContext.currentContext)
			}

//			"run A then Z 0msec で　ContextTimeoutする" in {
//				val id = UUID.randomUUID().toString
//				val input = "A!Z"
//				val json = """
//						{"A": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l"
//							},
//						"Z": 
//							{
//								"_kind": "sh",
//								"_main": "pwd",
//								"__finallyTimeout":"0"
//							}
//						}
//						"""
//
//				val parser = new MondogrossoProcessParser(id, input, json)
//				val result = parser.parseProcess
//
//				val identity = UUID.randomUUID().toString
//				val currentContext = new ProcessContext(identity, result)
//				
//				//コンテキストからの実行開始
//				currentContext.runContext
//
//				//Timeout処理の待ち
//				val waiter = new DummyParent()
//				waiter.waitTime(100)
//				
//				//即座にContextTimeoutが発生する
//				currentContext.currentStatus must be_==(ContextStatus.STATUS_TIMEOUTED)
//
//				//Zの実行記録がある
//				currentContext.currentContext.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
//					OrderPrefix._kind.toString,
//					OrderPrefix._main.toString,
//					OrderPrefix.__finallyTimeout.toString,
//					OrderPrefix._result.toString
//					))
//					
//				//最終的なContextに特定の値が含まれる
//				println("currentContext.currentContext	A to Z Timeout	"+currentContext.currentContext)
//			}

//			"run A,B,Z" in {
//				val id = UUID.randomUUID().toString
//				val input = "A>B!Z"
//				val json = """
//						{"A": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l"
//							},
//						"B": 
//							{
//								"_kind": "sh",
//								"_main": "grep"
//								"in" : "should be grep"
//							},
//						"Z": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l",
//								"__finallyTimeout":"10000"
//							}
//						}
//						"""
//
//				val parser = new MondogrossoProcessParser(id, input, json)
//				val result = parser.parseProcess
//
//				val identity = UUID.randomUUID().toString
//				val currentContext = new ProcessContext(identity, result)
//				
//				currentContext.runContext
//				
//				//Aの実行、Bの実行、Finallyの実行は非同期なので、待つ
//				val waitor = new DummyParent()
//				waitor.waitTime(1000)
//				
//				//実行順が入っているはず
//				println("runningProcessList	"+currentContext.runningProcessList)
//				
//				"not yet applied" must be_==("")
//			}

//			"run A,B(A:_result:in),Z" in {
//				val id = UUID.randomUUID().toString
//				val input = "A>B(A:_result:in)!Z"
//				val json = """
//						{"A": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l"
//							},
//						"B": 
//							{
//								"_kind": "sh",
//								"_main": "grep"
//								"in" : "should be grep"
//							},
//						"Z": 
//							{
//								"_kind": "sh",
//								"_main": "ls -l",
//								"__finallyTimeout":"10000"
//							}
//						}
//						"""
//
//				val parser = new MondogrossoProcessParser(id, input, json)
//				val result = parser.parseProcess
//
//				val identity = UUID.randomUUID().toString
//				val currentContext = new ProcessContext(identity, result)
//
//				currentContext.runContext
//				
//				//Aの実行、Bの実行、Finallyの実行は非同期なので、待つ
//				val waiter = new DummyParent()
//				waiter.waitTime(1000)
//				
//				//実行順が入っているはず
//				println("runningProcessList	"+currentContext.runningProcessList)
//				
//
//				
//				//BがAの_resultをgrepした結果を持つ
//				
//				"not yet applied" must be_==("")
//			}
		}
	}

	//Context Error
	if (true) {
		"Context エラー処理" should {

			"context生成時エラー　Finallyの__contexttimeout値がおかしい" in {

			}

			"timeoutエラー" in {
				"not yet applied" must be_==("")
			}

			"実行時エラー" in {
				"not yet applied" must be_==("")
			}

			"実行前エラー" in {
				"not yet applied" must be_==("")
			}
		}
	}
}