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
class MondogrossoProcessOrdersControllerTests extends Specification /*with TimeoutTrait*/ {

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
							"_main": "open /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/index.html"
						}
					}"""

	//OrderController	
	if (true) {
		"OrderController" should {
			
			/*
			 * コンテキスト周りのテスト
			 */
			"Contextを実行開始、成功結果を受け取る" in {
				val orderCont = new MondogrossoProcessOrdersController
				
				val id = UUID.randomUUID().toString
				val input = "A>B>C(A:a:c)<E+(B)D(A:a2:d1,A:a3:d2)>E!Z"
				val json = standardJSON

				val parser = new MondogrossoProcessParser(id, input, json)
				val parseResult = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = orderCont.attachProcess(identity, parseResult)

				//contextを生成
				val currentContext = orderCont.contexts(0)
				//現在実行中のOrder、内容がまだ無い
				orderCont.contexts(0).doingOrderIdentities.length must be_==(0)

				//起動
				orderCont.runAllContext

				while (!currentContext.currentStatus.head.equals(ContextStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("Contextを実行開始、すべて同期でくまれているので、一瞬で完了するはず	" + currentContext.currentStatus)
				}
				
				println("Contextを実行開始、すべて同期でくまれているので、一瞬で完了するはず	"+orderCont.contexts(0).currentContext)
				orderCont.contexts(0).currentContext.keys must be_==(Set("A","B","C","E","D","Z"))
				
				//結果を受け取る
				val contextResult = orderCont.contexts(0).result
				println("contextResult	"+contextResult)
				
				//結果は成功
				contextResult.status must be_==(ContextStatus.STATUS_DONE)
				
				//コメントを一覧で取得
				println("comments	"+contextResult.commentsStack)
			}
		}
	}
	
	
}