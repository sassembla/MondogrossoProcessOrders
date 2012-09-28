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
	}

	//Context
	if (true) {
		"Context run" should {
			"手順的に一つずつの手順ログを持つはず" in {
				"not yet applied" must be_==("")
			}

			"before run" in {
				val orderCont = new MondogrossoProcessOrdersController
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
								"__finallyTimeout":"10000"
							}
						}
						"""

				val parser = new MondogrossoProcessParser(id, input, json)
				val result = parser.parseProcess

				val identity = UUID.randomUUID().toString
				val s = orderCont.attachProcess(identity, result)

				val currentContext = orderCont.contexts(0)

				//run前、ContextのstatusがREADY
				currentContext.currentStatus must be_==(ContextStatus.STATUS_READY)
			}

			"run A then Z" in {
				val orderCont = new MondogrossoProcessOrdersController
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
								"__finallyTimeout":"10000"
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

				//run、ContextのstatusがRUNNING
				currentContext.currentStatus must be_==(ContextStatus.STATUS_RUNNING)

				//Aの実行、Finallyの実行は非同期なので、待つ
				val waitor = new DummyParent()
				waitor.waitTime(1000)

				currentContext.currentStatus must be_==(ContextStatus.STATUS_DONE)
				
				//Aの実行記録がある
				currentContext.currentContext.get("A").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				//Zの実行記録がある
				currentContext.currentContext.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix.__finallyTimeout.toString,
					OrderPrefix._result.toString,
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString))
			}

			"run A then Z ContextTimeoutする" in {
				val orderCont = new MondogrossoProcessOrdersController
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
								"__finallyTimeout":"1"
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

				//即座にContextTimeoutが発生する
				currentContext.currentStatus must be_==(ContextStatus.STATUS_ERROR)

				//Zの実行記録がある
				currentContext.currentContext.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__finallyTimeout,
					OrderPrefix._result.toString))
			}

			"run A,B,Z" in {
				val orderCont = new MondogrossoProcessOrdersController
				val id = UUID.randomUUID().toString
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
								"_main": "grep"
								"in" : "should be grep"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "ls -l",
								"__delay":"10000"
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
				val orderCont = new MondogrossoProcessOrdersController
				val id = UUID.randomUUID().toString
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
								"_main": "grep"
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

	//Worker
	if (true) {
		"Worker" should {

			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerを同期で実行後、実行完了したのでDone状態" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				//実行が同期的に行われ、ステータスがDONEになる
				worker.currentStatus must be_==(WorkerStatus.STATUS_DONE)
			}

			"Workerを同期で実行後、実行完了したので親にそのログが残る" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				val latestWork = worker.getLatestWorkInformation

				val currentFinishedWorkerIdentity = worker.workerIdentity
				val currentFinishedOrderIdentity = latestWork.orderIdentity

				//親側にlogが残る
				dummyParent.messenger.getLog.contains(currentFinishedWorkerIdentity + currentFinishedOrderIdentity) must beTrue
			}

			"Workerを同期で実行後、実行完了したので、次のOrderをリクエスト" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				//非同期なので、待ち
				dummyParent.waitTime(1000)

				val latestWork = worker.getLatestWorkInformation

				val processIdentity = worker.workerIdentity
				val finishedOrderIdentity = latestWork.orderIdentity

				//ダミーの親に、Orderのリクエスト通知がある
				dummyParent.messenger.getLog.contains(processIdentity + finishedOrderIdentity) must beTrue
			}
		}
	}

	//Worker Delay
	if (true) {
		"Worker　非同期" should { //安定しない、、 秒数指定が平気で0.5秒程度ずれる。何か有るな。

			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerを非同期で実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "1000"))))

				//非同期に待つ　この間に、非同期実行され、完了が親に届いているはず
				dummyParent.waitTime(1200) //1.2秒くらい待つ

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_DONE)

				val latestWork = worker.getLatestWorkInformation

				val currentFinishedWorkerIdentity = worker.workerIdentity
				val currentFinishedOrderIdentity = latestWork.orderIdentity

				//親に完了受信記録がある
				dummyParent.messenger.getLog.contains(currentFinishedWorkerIdentity + currentFinishedOrderIdentity) must beTrue

				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix._result.toString))

				//resultはその時々で変化するので割愛
			}

			"非同期のパラメータに数字以外を使用" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "wrong expression of number"))))

				//非同期のセット自体が非同期なので、待ち
				dummyParent.waitTime(100)

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation

				val erroredWorkerIdentity = worker.workerIdentity
				val erroredOrderIdentity = latestWork.orderIdentity

				//親に完了受信記録がある
				dummyParent.messenger.getLog.contains(erroredWorkerIdentity + erroredOrderIdentity) must beTrue

				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.lang.NumberFormatException: For input string: \"wrong expression of number\"")
			}

			"非同期のパラメータに-数字を使用" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "-1000"))))

				//非同期のセット自体が非同期なので、待ち
				dummyParent.waitTime(100)

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation

				val erroredWorkerIdentity = worker.workerIdentity
				val erroredOrderIdentity = latestWork.orderIdentity

				//親に完了受信記録がある
				dummyParent.messenger.getLog.contains(erroredWorkerIdentity + erroredOrderIdentity) must beTrue

				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.lang.IllegalArgumentException: timeout value is negative")
			}
		}

	}

	//Worker Timeout
	if (true) {
		"Worker タイムアウト" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerを同期で実行、タイムアウト" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							"-i" -> "hereComes",
							"-t" -> "1000", //1秒かかる処理なので、確実にタイムアウトになる
							OrderPrefix.__timeout.toString -> "100")))) //0.1秒でtimeout

				//非同期に待つ　この間に、タイムアウトは親に届いているはず
				dummyParent.waitTime(1500) //1.5秒くらい待つ

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)

				val latestWork = worker.getLatestWorkInformation

				val timeoutedWorkerIdentity = worker.workerIdentity
				val timeoutedOrderIdentity = latestWork.orderIdentity

				//親にタイムアウトの受信記録がある
				dummyParent.messenger.getLog.contains(timeoutedWorkerIdentity + timeoutedOrderIdentity) must beTrue

				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					"-i",
					"-t",
					OrderPrefix.__timeout.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 100msec elapsed")
			}

			"Workerを同期で実行、実際に時間のかかる処理でタイムアウト" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							"-i" -> "hereComes",
							"-t" -> "1500", //1.5秒かかる処理なので、確実にタイムアウトになる
							OrderPrefix.__timeout.toString -> "1000"))))

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					"-i",
					"-t",
					OrderPrefix.__timeout.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 1000msec elapsed")
			}

			"Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "10000",
							OrderPrefix.__timeout.toString -> "100"))))

				//非同期に待つ
				dummyParent.waitTime(1000) //1秒くらい待つ

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix.__timeout.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 100msec elapsed")
			}

			"Workerを非同期で実行、タイムアウト delayがtimeoutより若干大きい = 追い付く可能性" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "300",
							OrderPrefix.__timeout.toString -> "100"))))

				//非同期に待つ
				dummyParent.waitTime(1000) //1秒くらい待つ

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_TIMEOUT)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix.__timeout.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 100msec elapsed")
			}
		}
	}

	//Worker Cancel
	if (true) {
		"Worker Cancel" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"同期でのタイムアウトのキャンセル" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__timeout.toString -> "1000"))))

				//キャンセル

				//無駄なハズ
				"not yet applied" must be_==("")
			}

			"非同期でのタイムアウトのキャンセル" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "0",
							OrderPrefix.__timeout.toString -> "1000"))))

				"not yet applied" must be_==("")
			}

			"非同期のキャンセル" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "1000"))))

				"not yet applied" must be_==("")
			}
		}
	}

	//Worker Error
	if (true) {
		"Worker エラー" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"__timeoutの値がセットされていない、実行前エラー" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__timeout.toString -> "")))) //値の指定忘れ

				//この時点でエラー
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__timeout.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.lang.NumberFormatException: For input string: \"\"")
			}

			"_main,_typeという最低限のパラメータが足りない 実行前エラー __timeoutなし" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map("a" -> "b")))) //must値の指定忘れ

				//この時点でエラー
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					"a",
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b))")
			}

			"_main,_typeという最低限のパラメータが足りない 実行前エラー __timeoutあり" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"), //必須な_main、_kind値の指定忘れ
						new TagValue("context", Map("a" -> "b",
							OrderPrefix.__timeout.toString -> "1"))))

				//この時点でエラー
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix.__timeout.toString,
					"a",
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b, __timeout -> 1))")

			}

			"未知のtypeが送られてきた 実行前エラー" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "undefined type!",
							OrderPrefix._main.toString -> "ls -l"))))

				//この時点でエラー
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("_kind : undefined type! is not defined yet. please use WorkKinds.ValueSet(jar, sh, KIND_NOT_FOUND)")
			}

			"Workerを同期で実行、実行時エラー" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで実行エラー

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
			}

			"Workerを非同期で実行、実行時エラー" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで非同期下のエラー

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
			}
		}
	}

	//Worker OrderRun
	if (true) {
		"Worker Order実行" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerでshellを実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				//ls -lを実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				//結果はフォルダとかによって変化するため精査しない
			}

			"Workerでshellを実行2" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "open",
							"-a" -> "Safari.app /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersControllerTests.html"))))

				//ls -lを実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					"-a",
					OrderPrefix._result.toString))

				//結果はフォルダとかによって変化するため精査しない
			}

			"Workerで非同期のshellを実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> ""))))

				//非同期に待つ
				dummyParent.waitTime(1000) //1秒くらい待つ

				//ls -lを実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					OrderPrefix._result.toString))

				//結果はフォルダとかによって変化するため精査しない
			}

			"WorkerでJavaを実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							"-i" -> "hereComes"))))

				//java -jar TestProject.jar -i herecomes を実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					"-i",
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes\n")
			}

			"Workerで非同期なJavaを実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "1",
							"-i" -> "hereComes"))))

				//非同期に待つ
				dummyParent.waitTime(1000) //1秒くらい待つ

				//java -jar TestProject.jar -i herecomes を非同期に実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					"-i",
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes\n")
			}

			"Workerで非同期なJavaを実行2" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "1",
							"-i" -> "hereComes",
							"-t" -> "1000" //1秒後に答えが出る、そういうJar
							))))

				//非同期に待つ
				dummyParent.waitTime(1500) //1.5秒くらい待つ

				//java -jar TestProject.jar -i herecomes -t,,,を実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					"-i",
					"-t",
					OrderPrefix._result.toString))

				//結果はタイムスタンプによって変化するため精査しない
			}
		}
	}

	//Worker OrderRun Wait
	if (true) {
		"Worker Order実行　Wait付き" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerにwaitが存在するOrderを渡し、待たせる" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "0",
							"-i" -> "hereComes",
							"-t" -> "1000" //1秒後に答えが出る、そういうJar
							))))

				//Workerは待ちに入っているはず
				worker.currentStatus must be_==(WorkerStatus.STATUS_WAITING)

				val latestWork = worker.getLatestWorkInformation

				"not yet applied" must be_==("")
			}
		}
	}

	//TEST_HELP
	{
		"Test Helping" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"テスト補助、結果ページを表示" in {

				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "open",
							"-a" -> "Safari.app /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersControllerTests.html"))))
				//終わればOK
			}

		}
	}
}