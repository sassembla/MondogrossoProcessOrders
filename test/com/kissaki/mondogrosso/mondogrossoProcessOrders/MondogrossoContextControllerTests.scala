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

//timeoutTrait　なのだが、ちょっと調整が必要。必ず終了時間が一定になってしまう。 あと、系全体が2回走る。
//trait TimeoutTrait extends AroundExample with MustMatchers with TerminationMatchers with CommandLineArguments {
//
//	lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)
//	def timeout = commandLineTimeOut.getOrElse(2500.millis)
//
//	def around[T <% Result](t : => T) = {
//		lazy val result = t
//		val termination = result must terminate[T](sleep = timeout).orSkip((ko : String) => "TIMEOUT: " + timeout)
//		termination.toResult and result
//	}
//}

@RunWith(classOf[JUnitRunner])
class MondogrossoContextControllerTests extends Specification /*with TimeoutTrait*/ {
	val standardInput = "A>B>C(A:a:c)+(B)D(A:a2:d1,A:a3:d2)!Z"
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
							"_main": "open /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/index.html",
							"__finallyTimeout":"2000"
						}
					}"""

	/*
	 * OrderControllerへの単体Contextの投入	
	 */
	if (false) {
		"OrderController" should {

			/*
			 * Contextの実行から終了 
			 */
			"Contextを実行開始、成功結果を受け取る" in {
				val contextCont = new MondogrossoContextController
				
				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)<E+(B)D(A:a2:d1,A:a3:d2)>E!Z"

				val parser = new MondogrossoProcessParser(id, input, standardJSON)
				val parseResult = parser.parseProcess

				val identity = UUID.randomUUID().toString
				contextCont.attachProcessOrders(identity, parseResult)
				
				//起動
				contextCont.runAllContext
				
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY)) {
					Thread.sleep(100)
					println("wait	Contextを実行開始、成功結果を受け取る")
				}
				
				//特定のContextの結果だけを得る
				println("contextCont	"+contextCont.currentResultsOfContext(identity))
				contextCont.currentResultsOfContext(identity).length must be_==(1)
				
				//結果がDONE
				contextCont.currentResultsOfContext(identity)(0).status must be_==(ContextStatus.STATUS_DONE)
			}
		}	
	}

	if (false) {

		"OrderController コンテキスト実行周りのテスト" should {
			"Contextを実行開始、各Contextが成功結果を受け取る" in {
				val contextCont = new MondogrossoContextController

				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)<E+(B)D(A:a2:d1,A:a3:d2)>E!Z"

				val parser = new MondogrossoProcessParser(id, input, standardJSON)
				val parseResult = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = contextCont.attachProcessOrders(identity, parseResult)

				//contextを生成
				val currentContext = contextCont.activeContexts(0)
				
				//現在実行中のOrder、内容がまだ無い
				contextCont.activeContexts(0).doingOrderIdentities.length must be_==(0)

				//起動
				contextCont.runAllContext

				while (!currentContext.currentStatus.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("Contextを実行開始、すべて同期でくまれているので、一瞬で完了するはず	" + currentContext.status)
				}

				println("Contextを実行開始、すべて同期でくまれているので、一瞬で完了するはず	" + contextCont.activeContexts(0).contextKeyValues)
				contextCont.activeContexts(0).contextKeyValues.keys must be_==(Set("A", "B", "C", "E", "D", "Z"))

				//結果を受け取る
				val contextResult = contextCont.activeContexts(0).currentContextResult

				//結果は成功
				contextResult.status must be_==(ContextStatus.STATUS_DONE)

				//コメントを一覧で取得
				println("comments	" + contextResult.commentsStack)
			}

			"Contextを実行開始 Eのロックが解かれないので、達成不可能なオーダー順。　タイムアウトで失敗 結果を受け取る" in {
				val contextCont = new MondogrossoContextController

				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)<E>E+(B)D(A:a2:d1,A:a3:d2)!Z"

				val parser = new MondogrossoProcessParser(id, input, standardJSON)
				val parseResult = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = contextCont.attachProcessOrders(identity, parseResult)

				//contextを生成
				val currentContext = contextCont.activeContexts(0)
				//現在実行中のOrder、内容がまだ無い
				contextCont.activeContexts(0).doingOrderIdentities.length must be_==(0)

				//起動
				contextCont.runAllContext

				while (!currentContext.status.head.equals(ContextStatus.STATUS_TIMEOUTED)) {
					Thread.sleep(100)
					println("Contextを実行開始、失敗結果を受け取る	" + currentContext.status)
				}

				//結果を受け取る
				val contextResult = contextCont.activeContexts(0).currentContextResult

				//結果は失敗
				contextResult.status must be_==(ContextStatus.STATUS_TIMEOUTED)

				//コメントを一覧で取得
				println("timeout	comments	" + contextResult.commentsStack)

			}

			"Contextを実行開始 不正確な値引き継ぎエラーが出る。　エラー結果を受け取る" in {
				val contextCont = new MondogrossoContextController

				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)+(B)D(Error:a2:d1,A:a3:d2)!Z"
				
				val parser = new MondogrossoProcessParser(id, input, standardJSON)
				val parseResult = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = contextCont.attachProcessOrders(identity, parseResult)

				//contextを生成
				val currentContext = contextCont.activeContexts(0)
				//現在実行中のOrder、内容がまだ無い
				contextCont.activeContexts(0).doingOrderIdentities.length must be_==(0)

				//起動
				contextCont.runAllContext

				while (!currentContext.status.head.equals(ContextStatus.STATUS_ERROR)) {
					Thread.sleep(100)
					println("Contextを実行開始 エラーが出る。　エラー結果を受け取る	" + currentContext.status)
				}

				//結果を受け取る
				val contextResult = contextCont.activeContexts(0).currentContextResult

				//結果は失敗
				contextResult.status must be_==(ContextStatus.STATUS_ERROR)

				//コメントを一覧で取得
				println("error	comments	" + contextResult.commentsStack)

			}
		}
	}
	
	/*
	 * OrderControllerへの複数Contextの同時投入	
	 */
	if (false) {
		"複数のContextを同時に開始" should {
			"Context C1,C2を同時に開始" in {
				val contextCont = new MondogrossoContextController
				
				Seq("C1","C2").foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}
				
				//2つが準備中
				contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(2)
				
				//起動
				contextCont.runAllContext
				
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY)) {
					Thread.sleep(100)
					println("wait	Context C1,C2を同時に開始")
				}
				
				//2つ終了状態のContextがあるので、状態からContext名が取得できる
				contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)
				
				//２つ結果が有る
				contextCont.currentResults.length must be_==(2)
			}
		}
	}
	
	/*
	 * Contextの名前空間の重複を考慮する
	 */
	if (true) {
		"同じ名称のContextを大量に作って実行" should {
			"C1 x ２個" in {
				val contextCont = new MondogrossoContextController
				val C1 = UUID.randomUUID.toString

				Seq(C1,C1).foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}

				//コンテキスト実行
				contextCont.runAllContext

				
			}

			"C1 x n個" in {
				"not yet applied" must be_==("")
				
			}

			"C1,C2 x ２個" in {
				"not yet applied" must be_==("")

			}

			"C1,C2 x n個" in {
				"not yet applied" must be_==("")

			}
		}
	}

	/*
	 * OrderControllerへの複数Contextの順次投入
	 */
	if (false) {
		"複数のContextを順次導入" should {
			"C1投入後、C2を投入" in {
				val contextCont = new MondogrossoContextController

				Seq("C1").foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
	
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}
				
				//1つが準備中
				contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)

				//起動
				contextCont.runAllContext
				
				Seq("C2").foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
	
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}

				// //1つが準備中、一つが実行中(どのstateかは不明)
				contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)
				

				//起動2
				contextCont.runAllContext
				
				//待つ
				var i = 0
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY) && i < 50) {
					Thread.sleep(100)
					i+=1
					println("wait	C1投入後、C2を投入")
				}

				//2つ終了状態のContextがあるので、状態からContext名が取得できる
				contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)
				
				//２つ結果が有る
				contextCont.currentResults.length must be_==(2)
			}
			


			"C1投入後、完了を待ってC2を投入" in {
				val contextCont = new MondogrossoContextController

				
				Seq("C1").foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
	
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}
				
				//1つが準備中
				contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)

				//起動
				contextCont.runAllContext
				
				//待つ1
				var i = 0
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY) && i < 10) {
					Thread.sleep(100)
					println("wait	C1投入後、完了を待ってC2を投入	1")
					i+=1
				}

				println("その１、i is "+i)

				Seq("C2").foreach {contextIdentity =>
					val id = UUID.randomUUID().toString
	
					val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
					val parseResult = parser.parseProcess
	
					contextCont.attachProcessOrders(contextIdentity, parseResult)
				}

				//起動2
				contextCont.runAllContext
				
				//待つ2
				i = 0
				while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY) && i < 10) {
					Thread.sleep(100)
					i+=1
					println("wait	C1投入後、完了を待ってC2を投入	2")
				}

				println("その2、i is "+i)

				//2つ終了状態のContextがあるので、状態からContext名が取得できる
				contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)
				
				//２つ結果が有る
				contextCont.currentResults.length must be_==(2)
			}
			
			// "C1投入後/実行、C2を投入後/実行、C3を投入" in {
			// 	val contextCont = new MondogrossoContextController

			// 	Seq("C1").foreach {contextIdentity =>
			// 		val id = UUID.randomUUID().toString
	
			// 		val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
			// 		val parseResult = parser.parseProcess
	
			// 		contextCont.attachProcessOrders(contextIdentity, parseResult)
			// 	}
				
			// 	//起動1
			// 	contextCont.runAllContext
				
			// 	Seq("C2").foreach {contextIdentity =>
			// 		val id = UUID.randomUUID().toString
	
			// 		val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
			// 		val parseResult = parser.parseProcess
	
			// 		contextCont.attachProcessOrders(contextIdentity, parseResult)
			// 	}

			// 	//起動2
			// 	contextCont.runAllContext

			// 	Seq("C3").foreach {contextIdentity =>
			// 		val id = UUID.randomUUID().toString
	
			// 		val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
			// 		val parseResult = parser.parseProcess
	
			// 		contextCont.attachProcessOrders(contextIdentity, parseResult)
			// 	}

			// 	//起動3
			// 	contextCont.runAllContext
				
			// 	//待つ
			// 	var i = 0
			// 	while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY) && i < 10) {
			// 		Thread.sleep(100)
			// 		i+=1
			// 		println("wait	C1投入後/実行、C2を投入後/実行、C3を投入")
			// 	}

			// 	//3つ終了状態のContextがあるので、状態からContext名が取得できる
			// 	contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(3)
				
			// 	//3つ結果が有る
			// 	contextCont.currentResults.length must be_==(3)
			// }
		}
	}

}