package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID
import com.kissaki.TagValue
import scala.collection.mutable.ListBuffer
import org.specs2.main.CommandLineArguments
import org.specs2.matcher.TerminationMatchers
import org.specs2.specification.AroundExample
import org.specs2.matcher.MustMatchers
import org.specs2.execute.Result
import org.specs2.time.TimeConversions._

//timeoutTrait　なのだが、ちょっと調整が必要。必ず終了時間が一定になってしまう。
trait TimeoutTrait extends AroundExample with MustMatchers with TerminationMatchers with CommandLineArguments {

  lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)
  def timeout = commandLineTimeOut.getOrElse(2500.millis)

  def around[T <% Result](t: =>T) = {
    lazy val result = t
    val termination = result must terminate[T](sleep = timeout).orSkip((ko: String) => "TIMEOUT: "+timeout)
    termination.toResult and result
  }
}

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersControllerTests extends Specification with TimeoutTrait {
	
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
								"_main": "open",
								"-a":"Safari.app /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessWorkerTests.html"
							}
						}"""

		
		
	//OrderController	
	if (true) {
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
	if (true) {
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

		"手順的に一つずつの手順ログを持つはず" in {
			"not yet applied" must be_==("")
		}

		"before run" in {

			val id = UUID.randomUUID().toString
			val input = "A!Z"
			val json = """
					{"A": 
						{
							"_kind": "sh",
							"_main": "ls -l"
						},
					"Z": 
						{
							"_kind": "sh",
							"_main": "pwd",
							"__finallyTimeout":"100"
						}
					}
					"""

			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parseProcess

			val identity = UUID.randomUUID().toString
			val currentContext = new ProcessContext(identity, result)

			//run前、ContextのstatusがREADY
			currentContext.currentStatus.head must be_==(ContextStatus.STATUS_READY)
		}

		
	}

	//Context
	if (true) {
		"Context run" should {
			"Context 基礎的な挙動　run A then Z" in {
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

				val identity = "Context 基礎的な挙動　run A then Z" //UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext

				
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
				}
				
				//実行履歴
				currentContext.currentStatus must be_==(ListBuffer(ContextStatus.STATUS_DONE,
					ContextStatus.STATUS_FINALLY,
					ContextStatus.STATUS_RUNNING,
					ContextStatus.STATUS_READY,
					ContextStatus.STATUS_NOTREADY))

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
			}
		}
	}
	if (true) {
		"Context タイムアウトについて" should {
			"1:run A then Z finallyTimeout付きで時間内に完了する" in {
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
								"__finallyTimeout":"8000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = "1:run A then Z finallyTimeout付きで時間内に完了する" //UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext

				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
				}

				//実行履歴
				currentContext.currentStatus must be_==(ListBuffer(ContextStatus.STATUS_DONE,
					ContextStatus.STATUS_FINALLY,
					ContextStatus.STATUS_RUNNING,
					ContextStatus.STATUS_READY,
					ContextStatus.STATUS_NOTREADY))
			}
			
			"2:run A then Z 1msec で　ContextTimeoutする" in {
				val id = UUID.randomUUID().toString
				val input = "A!Z"
				val json = """
						{"A": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "このJarが実行されたのは3:run A then Z 1msec で　ContextTimeoutする です",
								"-t" : "1000"
								
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__finallyTimeout":"1"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess
				
				val identity = "2:run A then Z 1msec で　ContextTimeoutする" //UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext

				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_TIMEOUTED)) {
					Thread.sleep(100)
				}

				//実行履歴
				currentContext.currentStatus must be_==(ListBuffer(ContextStatus.STATUS_TIMEOUTED,
					ContextStatus.STATUS_TIMEOUT,
					ContextStatus.STATUS_RUNNING,
					ContextStatus.STATUS_READY,
					ContextStatus.STATUS_NOTREADY))
			}
			
			"3:run A then Z 0msec で正常終了" in {
				val id = UUID.randomUUID().toString
				val input = "A!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = "3:run A then Z 0msec で正常終了" //UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				//コンテキストからの実行開始
				currentContext.runContext

				//Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
				}

				//実行履歴
				currentContext.currentStatus must be_==(ListBuffer(ContextStatus.STATUS_DONE,
					ContextStatus.STATUS_FINALLY,
					ContextStatus.STATUS_RUNNING,
					ContextStatus.STATUS_READY,
					ContextStatus.STATUS_NOTREADY))
			}
		}
	}

	if (true) {
		"Context 複雑なOrder" should {
			"5:run A,B,Z 複数のOrder" in {
				val id = UUID.randomUUID().toString
				val input = "A>B!Z"//なんでもgrep
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep",
								"\"a\"" : "*.*"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("5")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				"not yet applied" must be_==("")
			}

			"6:run A,B(A:_result:in),Z 複数のOrderで値を適応" in {
				val id = UUID.randomUUID().toString
				val input = "A>B(A:_result:in)!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo *.*"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"\"a\"" : "should be grep"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				//	Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("6")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				//BがAの_resultをgrepした結果を持つ

				"not yet applied" must be_==("")
			}

			"6.5:複数のOrderで値を共有する" in {
				val id = UUID.randomUUID().toString
				val input = "A>B(A:_result:b)>C(A:_result:c)!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"b" : "should be grep of A's result"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": ""
								"echo" : "should be address"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				//	Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("6.5")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				//各処理の結果が入っているはず

				"not yet applied" must be_==("")
			}

			"7:時間のかかる処理を並列で行う" in {
				val id = UUID.randomUUID().toString
				val input = "B+B>C+B>D+B>E!Z"
				val json = """
						{
						"B": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7B",
								"-t" : "1000"
								
							},
						"C": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Cです",
								"-t" : "1000"
								
							},
						"D": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Dです",
								"-t" : "1000"
								
							},
						"D": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Eです",
								"-t" : "1000"
								
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l"
							}
						}
						"""
				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				println("入れ替えマップが用意された状態のcurrent	"+currentContext.currentContext)
				//	Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("7")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				//各処理の結果が入っているはず

				"not yet applied" must be_==("")
			}
			
			
			"8:待ちが存在するOrder Aが完了したらBが動き出す" in {
				
				val id = UUID.randomUUID().toString
				val input = "A+A>B(A:_result:in)!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo *.*"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"\"a\"" : "should be grep"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				//	Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("8")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				//BがAの_resultをgrepした結果を持つ

				"not yet applied" must be_==("")

			}
			
			"9:待ちが存在するOrder Aの値を継いで、Aが完了したらBが動き出す" in {
				
				val id = UUID.randomUUID().toString
				val input = "A+A>B(A:_result:in)!Z"
				val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo *.*"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"\"a\"" : "should be grep"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess
				println("開始されているはず	result	"+result)
				val identity = UUID.randomUUID().toString
				val currentContext = new ProcessContext(identity, result)

				currentContext.runContext

				//	Timeout処理の待ち
				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("9")
				}

				//実行順が入っているはず
				println("runningProcessList	" + currentContext.runningProcessList)

				//BがAの_resultをgrepした結果を持つ

				"not yet applied" must be_==("")

			}
			
			
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