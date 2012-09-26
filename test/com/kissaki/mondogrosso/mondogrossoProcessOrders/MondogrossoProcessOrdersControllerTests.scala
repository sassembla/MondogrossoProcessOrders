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
								"_main": "ls -l"
							}
						}
						"""

	"OrderController" should {
		"attachされていて	まだ実行されていない	コンディションのContextを実行開始" in {
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

	"Context information" should {
		val orderCont = new MondogrossoProcessOrdersController

		val id = UUID.randomUUID().toString
		val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
		val json = standardJSON

		val parser = new MondogrossoProcessParser(id, input, json)
		val result = parser.parseProcess

		val identity = UUID.randomUUID().toString
		val s = orderCont.attachProcess(identity, result)

		val currentContext = orderCont.contexts(0)

		"Contextを生成した時点で、Context内での値はすべてSequenceとして存在しているはず" in {
			//プロセス数を取得(=で将来作られるWorker数)
			currentContext.processNum must be_==(2)

			//全体インデックスを取得
			currentContext.totalOrderNum must be_==(6)

			//現在の進捗インデックスを取得
			currentContext.currentOrderIndex must be_==(0)
		}

		"最初から実行	現在実行中のOrderがAなハズ" in {
			//			currentContext.run()
			//			println("currentContext.currentExecutingOrders(0)	" + currentContext.currentExecutingOrders(0))
			//			currentContext.currentExecutingOrders(0) must be_==("A")
			"not yet applied" must be_==("")
		}

		"途中のindexから実行" in {
			//			currentContext.startFrom(2)
			//			
			//			//現在実行中のOrderがAなハズ
			//			currentContext.currentExecutingOrders must be_==(Map("A"))

			"not yet applied" must be_==("")
		}
	}
	
	"Context run" should {
		val orderCont = new MondogrossoProcessOrdersController

		val id = UUID.randomUUID().toString
		
		
		"run A then Z" in {
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
								"_main": "ls -l"
							}
						}
						"""
	
			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parseProcess
	
			val identity = UUID.randomUUID().toString
			val s = orderCont.attachProcess(identity, result)
	
			val currentContext = orderCont.contexts(0)
		
			//コンテキストからの実行開始
			currentContext.runContext
			
			//同期で準備されているのでそのままA > Finallyと実行される
			
			//Aの実行記録がある
			currentContext.currentContext.get("A").getOrElse(Map()).keys must be_==(Set(
				"_kind",
				"_main",
				"_result"))
			
			//Zの実行記録がある
			currentContext.currentContext.get("Z").getOrElse(Map()).keys must be_==(Set(
				"_kind",
				"_main",
				"_result"))
				
			"not yet applied" must be_==("")
		}
		
		"run A,B,Z" in {
			val input = "A>B!Z"
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
								"in" : "should be grep"
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
			val s = orderCont.attachProcess(identity, result)
	
			val currentContext = orderCont.contexts(0)
			
			"not yet applied" must be_==("")
		}
		
		"run A,B(A:_result:in),Z" in {
			val input = "A>B(A:_result:in)!Z"
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
								"in" : "should be grep"
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
			val s = orderCont.attachProcess(identity, result)
	
			val currentContext = orderCont.contexts(0)
			
			//BがAの_resultをgrepした結果を持つ
			
			"not yet applied" must be_==("")
		}
	}
	
	"Context エラー処理" should {
		
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
	
	

	"Worker" should {

		//擬似的に親代わりを生成する
		val dummyParent = new DummyParent()

		val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
		val json = standardJSON

		//Workerは、担当するProcessを与えられる

		"Workerを同期で実行後、実行完了したのでDone状態" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l"))))

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_DONE)
		}
		
		"Workerを同期で実行、タイムアウト" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"__timeout" -> "1000"))))

			//非同期に待つ　この間に、タイムアウトは親に届いているはず
			dummyParent.waitTime(2000) //2秒くらい待つ

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)
			
			//親にタイムアウトの受信記録がある
			dummyParent.messenger.getLog.contains("MESSAGE_TIMEOUT") must beTrue
			
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"__timeout",
				"_result"))
				
			latestWork.localContext.get("_result").getOrElse("...empty") must be_==("timeout 1000msec elapsed")
		}
		
		"Workerを同期で実行、実際に時間のかかる処理でタイムアウト" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "jar",
						"_main" -> "TestProject",
						"-i" -> "hereComes",
						"-t" -> "2000",//2秒かかる処理なので、確実にタイムアウトになる
						"__timeout" -> "1000"))))

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)
			
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"-i",
				"-t",
				"__timeout",
				"_result"))
				
			latestWork.localContext.get("_result").getOrElse("...empty") must be_==("timeout 1000msec elapsed")
		}
		
		"Workerを非同期で実行、タイムアウト" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"__async" -> "no key will be appear.",
						"__timeout" -> "1000"))))

			//非同期に待つ
			dummyParent.waitTime(1200) //1.2秒くらい待つ

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)
			
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"__async",
				"__timeout",
				"_result"))
			
			latestWork.localContext.get("_result").getOrElse("...empty") must be_==("timeout 1000msec elapsed")
		}
		
		"パラメータが足りない 実行前エラー" in {
			"not yet applied" must be_==("")
		}
		
		"未知のtypeが送られてきた 実行前エラー" in {
			"not yet applied" must be_==("")
		}
		
		"Workerを同期で実行、実行時エラー" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "dfghjklls -l"))))//存在しないコマンドで実行エラー

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)
			
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"_result"))
				
			latestWork.localContext.get("_result").getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
		}
		
		"Workerを非同期で実行、実行時エラー" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "dfghjklls -l"))))//存在しないコマンドで非同期下のエラー

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)
			
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"_result"))
				
			latestWork.localContext.get("_result").getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
		}
		

		"Workerでshellを実行" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l"))))

			//ls -lを実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"_result"))
				
			//結果はフォルダとかによって変化するため精査しない
		}

		"Workerでshellを実行2" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "open",
						"-a" -> "Safari.app http://google.com"))))

			//ls -lを実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"-a",
				"_result"))
				
			//結果はフォルダとかによって変化するため精査しない
		}

		"Workerで非同期のshellを実行" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"__async" -> ""))))

			//非同期に待つ
			dummyParent.waitTime(1000) //1秒くらい待つ

			//ls -lを実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"__async",
				"_result"))
				
			//結果はフォルダとかによって変化するため精査しない
		}

		"WorkerでJavaを実行" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "jar",
						"_main" -> "TestProject",
						"-i" -> "hereComes"))))

			//java -jar TestProject.jar -i herecomes を実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"-i",
				"_result"))

			latestWork.localContext("_result") must be_==("over:hereComes\n")
		}

		"Workerで非同期なJavaを実行" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "jar",
						"_main" -> "TestProject",
						"__async" -> "on",
						"-i" -> "hereComes"))))

			//非同期に待つ
			dummyParent.waitTime(1000) //1秒くらい待つ

			//java -jar TestProject.jar -i herecomes を非同期に実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"__async",
				"-i",
				"_result"))
				
			latestWork.localContext("_result") must be_==("over:hereComes\n")
		}

		"Workerで非同期なJavaを実行2" in {
			val workerId = UUID.randomUUID().toString
			val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
			dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "jar",
						"_main" -> "TestProject",
						"__async" -> "on",
						"-i" -> "hereComes",
						"-t" -> "10000" //10秒後に答えが出る、そういうJar
						))))

			//非同期に待つ
			dummyParent.waitTime(11000) //11秒くらい待つ

			//java -jar TestProject.jar -i herecomes -t,,,を実行した結果が残っているはず
			val latestWork = worker.getLatestWorkInformation
			latestWork.localContext.keys must be_==(Set(
				"_kind",
				"_main",
				"__async",
				"-i",
				"-t",
				"_result"))
			
			//結果はタイムスタンプによって変化するため精査しない
		}

	}

}