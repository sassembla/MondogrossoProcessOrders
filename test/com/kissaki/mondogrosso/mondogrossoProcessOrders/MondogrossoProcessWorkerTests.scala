package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID
import com.kissaki.TagValue

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessWorkerTests extends Specification {

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
								"__delay":"100"
							}
						}"""

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				//実行が同期的に行われ、ステータスがDONEになる
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
			}

			"Workerを同期で実行後、実行完了したので親にそのログが残る" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l"))))

				//非同期なので、待ち
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("waiting,,,Workerを同期で実行後、実行完了したので、次のOrderをリクエスト	"+this)
				}

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
		"Worker　非同期" should {

			//擬似的に親を生成する
			val dummyParent = new DummyParent()

			"Workerを非同期で実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "1000"))))

				//非同期に待つ　この間に、非同期実行され、完了が親に届いているはず
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("waiting,,,"+this)
				}

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "wrong expression of number"))))

				//非同期のセット自体が非同期なので、待ち
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_ERROR)) {
					Thread.sleep(100)
					println("waiting,,,非同期のパラメータに数字以外を使用	"+this)
				}

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "-1000"))))

				//非同期のセット自体が非同期なので、待ち
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_ERROR)) {
					Thread.sleep(100)
					println("waiting,,,非同期のパラメータに-数字を使用	"+this)
				}

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

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.lang.IllegalArgumentException: Negative delay.")
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							"-i" -> "hereComes",
							"-t" -> "1000", //1秒かかる処理なので、確実にタイムアウトになる
							OrderPrefix.__timeout.toString -> "100")))) //0.1秒でtimeout

				//非同期に待つ　この間に、タイムアウトは親に届いているはず
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_TIMEOUT)) {
					Thread.sleep(100)
					println("waiting,,,Workerを同期で実行、タイムアウト	"+this)
				}

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							"-i" -> "hereComes",
							"-t" -> "1500", //1.5秒かかる処理なので、確実にタイムアウトになる
							OrderPrefix.__timeout.toString -> "1000"))))

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_TIMEOUT)

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "10000",
							OrderPrefix.__timeout.toString -> "100"))))

				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_TIMEOUT)) {
					Thread.sleep(100)
					println("waiting,,,Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい	"+this)
				}
				
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "300",
							OrderPrefix.__timeout.toString -> "100"))))

				//非同期に待つ
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_TIMEOUT)) {
					Thread.sleep(100)
					println("waiting,,,Workerを非同期で実行、タイムアウト delayがtimeoutより若干大きい = 追い付く可能性	"+this)
				}
				
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__timeout.toString -> "")))) //値の指定忘れ

				//この時点でエラー
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map("a" -> "b")))) //must値の指定忘れ

				//この時点でエラー
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					"a",
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b),List())")
			}

			"_main,_typeという最低限のパラメータが足りない 実行前エラー __timeoutあり" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"), //必須な_main、_kind値の指定忘れ
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map("a" -> "b",
							OrderPrefix.__timeout.toString -> "1"))))

				//この時点でエラー
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix.__timeout.toString,
					"a",
					OrderPrefix._result.toString))

				latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b, __timeout -> 1),List())")

			}

			"未知のtypeが送られてきた 実行前エラー" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "undefined type!",
							OrderPrefix._main.toString -> "ls -l"))))

				//この時点でエラー
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで実行エラー

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで非同期下のエラー

				//実行が同期的に行われ、実行されたあとの情報が残る
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "ls -l",
							OrderPrefix.__delay.toString -> "100"))))

				//非同期に待つ
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("waiting,,,Workerで非同期のshellを実行	"+this+"	worker.currentStatus.head	"+worker.currentStatus.head)
				}
							
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
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
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

				latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes")
			}

			"Workerで非同期なJavaを実行" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "1",
							"-i" -> "hereComes"))))

				//非同期に待つ
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("waiting,,,Workerで非同期なJavaを実行	"+this)
				}
				
				//java -jar TestProject.jar -i herecomes を非同期に実行した結果が残っているはず
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix.__delay.toString,
					"-i",
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes")
			}

			"Workerで非同期なJavaを実行2" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "1",
							"-i" -> "hereComes",
							"-t" -> "1000" //1秒後に答えが出る、そういうJar
							))))

				//非同期に待つ
				while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE)) {
					Thread.sleep(100)
					println("waiting,,,Workerで非同期なJavaを実行2	"+this)
				}

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

	//Worker processSplit Wait
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
						new TagValue("processSplitIds",List("WAIT")),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "jar",
							OrderPrefix._main.toString -> "TestProject",
							OrderPrefix.__delay.toString -> "0",
							"-i" -> "hereComes",
							"-t" -> "1000" //1秒後に答えが出る、そういうJar
							))))

				//WorkerはSTATUS_SPLIT_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_SPLIT_WAIT)

				val latestWork = worker.getLatestWorkInformation
			}
			
			"待ちが完了するシチュエーション" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List("WAIT")),
						new TagValue("afterWaitIds", List()),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはSTATUS_SPLIT_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_SPLIT_WAIT)
				
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "WAIT"),
								new TagValue("allfinishedOrderIdentities", List("WAIT"))))
				
				//WorkerはSTATUS_DONEになっているはず		
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
		}
	}
	
	//Order後のwait afterWaitについて
	if (true) {
		"AfterWait" should {
			//擬似的に親を生成する
			val dummyParent = new DummyParent()
			
			"waitに入ってからFinishedが来る" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List("B")),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはSTATUS_AFTER_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)
				
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(
								new TagValue("finishedOrderIdentity", "B"),
								new TagValue("allfinishedOrderIdentities", List("B"))))
				
				//WorkerはSTATUS_DONEになっているはず		
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
			
			"waitに入ってからFinishedが来る　複数のWait" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List("B","C")),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはSTATUS_AFTER_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)
				
				//B
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
								new TagValue("allfinishedOrderIdentities", List("B"))))
				
				//WorkerはまだSTATUS_AFTER_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)
				
				//C
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
								new TagValue("allfinishedOrderIdentities", List("B","C"))))
				
				//WorkerはSTATUS_DONEになっているはず	
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
			
			"waitに入る前にFinishedが来る" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				//Bが終わった事が伝わる
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
								new TagValue("allfinishedOrderIdentities", List("B"))))
				
				//この状態で開始
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List("B")),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはSTATUS_DONEになっているはず		
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
			
			"waitに入る前にFinishedが来る 複数" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				//Bが終わった事が伝わる
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
								new TagValue("allfinishedOrderIdentities", List("B"))))
						
				//Cが終わった事が伝わる
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
								new TagValue("allfinishedOrderIdentities", List("B","C"))))
				
				//この状態で開始
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List("B", "C")),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはSTATUS_DONEになっているはず		
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
			
			"waitに入る前、入った後にFinishedが来る 複数" in {
				val workerId = UUID.randomUUID().toString
				val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
				
				//Bが終わった事が伝わる
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
								new TagValue("allfinishedOrderIdentities", List("B"))))
						
				
				//この状態で開始
				dummyParent.messenger.call(workerId, Messages.MESSAGE_START.toString,
					dummyParent.messenger.tagValues(
						new TagValue("identity", "A"),
						new TagValue("processSplitIds",List()),
						new TagValue("afterWaitIds", List("B", "C")),
						new TagValue("context", Map(
							OrderPrefix._kind.toString -> "sh",
							OrderPrefix._main.toString -> "echo some"
							))))
				
				//WorkerはまだSTATUS_AFTER_WAITに入っているはず
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)
				
							
				//Cが終わった事が伝わる
				dummyParent.messenger.call(workerId, Messages.MESSAGE_FINISHEDORDER_NOTIFY.toString, 
						dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
								new TagValue("allfinishedOrderIdentities", List("B","C"))))
				
				//WorkerはSTATUS_DONEになっているはず		
				worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
				
				//Aの実行が完了している
				val latestWork = worker.getLatestWorkInformation
				latestWork.localContext.keys must be_==(Set(
					OrderPrefix._kind.toString,
					OrderPrefix._main.toString,
					OrderPrefix._result.toString))

				latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
			}
		}
	}
}