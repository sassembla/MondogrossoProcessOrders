package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID
import com.kissaki.TagValue
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnitRunner])
class ProcessWorkerTests extends Specification {

  /**
   * タイムアウト入りの完了ステータスを計る関数
   */
  def timeoutOrDoneOrAfterWait(identity: String, worker: ProcessWorker, dummyParent: DummyWorkerParent, limit: Int = 100) = {
    var i = 0
    println("OrDoneOrAfterWait開始	" + identity)
    while (!worker.currentStatus.head.equals(WorkerStatus.STATUS_DONE) &&
      !worker.currentStatus.head.equals(WorkerStatus.STATUS_TIMEOUT) &&
      !worker.currentStatus.head.equals(WorkerStatus.STATUS_AFTER_WAIT) &&
      !worker.currentStatus.head.equals(WorkerStatus.STATUS_ERROR) && i < limit) {
      i += 1
      println("OrDoneOrAfterWait waiting	" + identity + "	/" + i + " of " + limit)
      Thread.sleep(100)
    }
    if (limit == i) {
      dummyParent.outputLog
      sys.error("回数超過 " + identity)
      sys.exit(-1)
    }

    println("OrDoneOrAfterWait完了	" + identity)
  }

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
      "Workerを実行後、完了したのでDone状態" in {



        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを実行後、完了したのでDone状態")
        
        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
        	
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l"))))
          println("えっえっ	"+exec)
        }

        timeoutOrDoneOrAfterWait("Workerを実行後、完了したのでDone状態", worker, dummyParent)

        //実行され、ステータスがDONEになる
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)
        dummyParent.outputLog
      }

      // "Workerを実行後、完了したので親にそのログが残る" in {

      // 	//擬似的に親を生成する
      // 	val dummyParent = new DummyWorkerParent("Workerを実行後、完了したので親にそのログが残る")

      // 	val workerId = UUID.randomUUID().toString
      // 	val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

      // 	Seq(WorkerMessages.MESSAGE_SETUP.toString,WorkerMessages.MESSAGE_START.toString).foreach {exec =>
      // 		dummyParent.messenger.call(workerId, exec,
      // 			dummyParent.messenger.tagValues(
      // 				new TagValue("identity", "A"),
      // 				new TagValue("processSplitIds",List()),
      // 				new TagValue("afterWaitIds", List()),
      // 				new TagValue("context", Map(
      // 					OrderPrefix._kind.toString -> "sh",
      // 					OrderPrefix._main.toString -> "ls -l"))))
      // 	}

      // 	timeoutOrDoneOrAfterWait("Workerを実行後、完了したので親にそのログが残る", worker, dummyParent)

      // 	val latestWork = worker.getLatestWorkInformation

      // 	val currentFinishedWorkerIdentity = worker.workerIdentity
      // 	val currentFinishedOrderIdentity = latestWork.orderIdentity

      // 	//親側にlogが残る
      // 	dummyParent.messenger.getLog.contains(currentFinishedWorkerIdentity + currentFinishedOrderIdentity) must beTrue
      // 	dummyParent.outputLog
      // }

      // "Workerを実行後、完了したので、次のOrderをリクエスト" in {

      // 	//擬似的に親を生成する
      // 	val dummyParent = new DummyWorkerParent("Workerを実行後、完了したので、次のOrderをリクエスト")

      // 	val workerId = UUID.randomUUID().toString
      // 	val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
      // 	Seq(WorkerMessages.MESSAGE_SETUP.toString,WorkerMessages.MESSAGE_START.toString).foreach {exec =>
      // 		dummyParent.messenger.call(workerId, exec,
      // 			dummyParent.messenger.tagValues(
      // 				new TagValue("identity", "A"),
      // 				new TagValue("processSplitIds",List()),
      // 				new TagValue("afterWaitIds", List()),
      // 				new TagValue("context", Map(
      // 					OrderPrefix._kind.toString -> "sh",
      // 					OrderPrefix._main.toString -> "ls -l"))))
      // 	}

      // 	timeoutOrDoneOrAfterWait("Workerを実行後、完了したので、次のOrderをリクエスト", worker, dummyParent)

      // 	val latestWork = worker.getLatestWorkInformation

      // 	val processIdentity = worker.workerIdentity
      // 	val finishedOrderIdentity = latestWork.orderIdentity

      // 	//ダミーの親に、Orderのリクエスト通知がある
      // 	dummyParent.messenger.getLog.contains(processIdentity + finishedOrderIdentity) must beTrue
      // 	dummyParent.outputLog
      // }
    }
  }

  //Worker Delay
  if (false) {
    "Worker　非同期" should {

      "Workerを非同期で実行" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを非同期で実行")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "1000"))))
        }

        //非同期に待つ　この間に、非同期実行され、完了が親に届いているはず
        timeoutOrDoneOrAfterWait("Workerを非同期で実行", worker, dummyParent)

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
        dummyParent.outputLog
      }

      "非同期のパラメータに数字以外を使用" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("非同期のパラメータに数字以外を使用")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "wrong expression of number"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("非同期のパラメータに数字以外を使用", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
        dummyParent.outputLog
      }

      "非同期のパラメータに-数字を使用" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("非同期のパラメータに-数字を使用")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "-1000"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("非同期のパラメータに-数字を使用", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

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
        dummyParent.outputLog
      }
    }

  }

  //Worker Timeout
  if (false) {
    "Worker タイムアウト" should {

      "Workerを同期で実行、タイムアウト" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを同期で実行、タイムアウト")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                "-i" -> "hereComes",
                "-t" -> "1000", //1秒かかる処理なので、確実にタイムアウトになる
                OrderPrefix.__timeout.toString -> "100")))) //0.1秒でtimeout
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerを同期で実行、タイムアウト", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_TIMEOUT)

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
        dummyParent.outputLog
      }

      "実際に時間のかかる処理でタイムアウト" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("実際に時間のかかる処理でタイムアウト")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                "-i" -> "hereComes",
                "-t" -> "1500", //1.5秒かかる処理なので、確実にタイムアウトになる
                OrderPrefix.__timeout.toString -> "1000"))))
        }

        timeoutOrDoneOrAfterWait("実際に時間のかかる処理でタイムアウト", worker, dummyParent)

        //実行されたあとの情報が残る
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
        dummyParent.outputLog
      }

      "Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい")

        val workerId = "Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい" //UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "10000",
                OrderPrefix.__timeout.toString -> "100"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerを非同期で実行、タイムアウト delayがtimeoutより十分大きい", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_TIMEOUT)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix.__delay.toString,
          OrderPrefix.__timeout.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 100msec elapsed")
        dummyParent.outputLog
      }

      "Workerを非同期で実行、タイムアウト delayがtimeoutより若干大きい = 追い付く可能性" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを非同期で実行、タイムアウト delayがtimeoutより若干大きい = 追い付く可能性")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "300",
                OrderPrefix.__timeout.toString -> "100"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerを非同期で実行、タイムアウト delayがtimeoutより若干大きい = 追い付く可能性", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_TIMEOUT)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix.__delay.toString,
          OrderPrefix.__timeout.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("timeout 100msec elapsed")
        dummyParent.outputLog
      }
    }
  }

  //Worker Cancel
  if (false) {
    "Worker Cancel" should {

      "同期でのタイムアウトのキャンセル" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("同期でのタイムアウトのキャンセル")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__timeout.toString -> "1000"))))
        }

        //キャンセル

        //無駄なハズ
        "not yet applied" must be_==("")
        dummyParent.outputLog
      }

      "非同期でのタイムアウトのキャンセル" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("非同期でのタイムアウトのキャンセル")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "0",
                OrderPrefix.__timeout.toString -> "1000"))))
        }
        "not yet applied" must be_==("")
        dummyParent.outputLog
      }

      "非同期のキャンセル" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("非同期のキャンセル")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "1000"))))
        }

        "not yet applied" must be_==("")
        dummyParent.outputLog
      }
    }
  }

  //Worker Error
  if (false) {
    "Worker エラー" should {

      "__timeoutの値がセットされていない、実行前エラー" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("__timeoutの値がセットされていない、実行前エラー")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__timeout.toString -> "")))) //値の指定忘れ
        }

        //この時点でエラー
        worker.currentStatus.head must be_==("__timeoutの値がセットされていない、実行前エラー", WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix.__timeout.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.lang.NumberFormatException: For input string: \"\"")
        dummyParent.outputLog
      }

      "_main,_kindという最低限のパラメータが足りない 実行前エラー __timeoutなし" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("_main,_kindという最低限のパラメータが足りない 実行前エラー __timeoutなし")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map("a" -> "b")))) //must値の指定忘れ
        }

        //この時点でエラー
        worker.currentStatus.head must be_==("_main,_kindという最低限のパラメータが足りない 実行前エラー __timeoutなし", WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          "a",
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b),List())")
        dummyParent.outputLog
      }

      "_main,_kindという最低限のパラメータが足りない 実行前エラー __timeoutあり" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("_main,_kindという最低限のパラメータが足りない 実行前エラー __timeoutあり")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"), //必須な_main、_kind値の指定忘れ
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map("a" -> "b",
                OrderPrefix.__timeout.toString -> "1"))))
        }

        //この時点でエラー
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix.__timeout.toString,
          "a",
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("no _kind _main keys found in WorkInformation(A,Map(a -> b, __timeout -> 1),List())")
        dummyParent.outputLog

      }

      "未知のtypeが送られてきた 実行前エラー" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("未知のtypeが送られてきた 実行前エラー")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "undefined type!",
                OrderPrefix._main.toString -> "ls -l"))))
        }

        //この時点でエラー
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("_kind : undefined type! is not defined yet. please use WorkKinds.ValueSet(jar, sh, KIND_NOT_FOUND)")
        dummyParent.outputLog
      }

      "Workerを同期で実行、実行時エラー" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを同期で実行、実行時エラー")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで実行エラー
        }

        //実行が同期的に行われ、実行されたあとの情報が残る
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
        dummyParent.outputLog
      }

      "Workerを非同期で実行、実行時エラー" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerを非同期で実行、実行時エラー")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "dfghjklls -l")))) //存在しないコマンドで非同期下のエラー
        }

        //実行が同期的に行われ、実行されたあとの情報が残る
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_ERROR)

        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext.get(OrderPrefix._result.toString).getOrElse("...empty") must be_==("java.io.IOException: Cannot run program \"dfghjklls\": error=2, No such file or directory")
        dummyParent.outputLog
      }
    }
  }

  //Worker OrderRun
  if (false) {
    "Worker Order実行" should {

      "Workerでshellを実行" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerでshellを実行")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l"))))
        }

        timeoutOrDoneOrAfterWait("Workerでshellを実行", worker, dummyParent)

        //ls -lを実行した結果が残っているはず
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        //結果はフォルダとかによって変化するため精査しない
        dummyParent.outputLog
      }

      "Workerでshellを実行2" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerでshellを実行2")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "open",
                "-a" -> "Safari.app /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersControllerTests.html"))))
        }

        timeoutOrDoneOrAfterWait("Workerでshellを実行2", worker, dummyParent)

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

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerで非同期のshellを実行")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "ls -l",
                OrderPrefix.__delay.toString -> "100"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerで非同期のshellを実行", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //ls -lを実行した結果が残っているはず
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix.__delay.toString,
          OrderPrefix._result.toString))

        //結果はフォルダとかによって変化するため精査しない
        dummyParent.outputLog
      }

      "WorkerでJavaを実行" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("WorkerでJavaを実行")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                "-i" -> "hereComes"))))
        }

        timeoutOrDoneOrAfterWait("WorkerでJavaを実行", worker, dummyParent)

        //java -jar TestProject.jar -i herecomes を実行した結果が残っているはず
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          "-i",
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes")
        dummyParent.outputLog
      }

      "Workerで非同期なJavaを実行" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerで非同期なJavaを実行")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                OrderPrefix.__delay.toString -> "1",
                "-i" -> "hereComes"))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerで非同期なJavaを実行", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //java -jar TestProject.jar -i herecomes を非同期に実行した結果が残っているはず
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix.__delay.toString,
          "-i",
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("over:hereComes")
        dummyParent.outputLog
      }

      "Workerで非同期なJavaを実行2" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("Workerで非同期なJavaを実行2")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                OrderPrefix.__delay.toString -> "1",
                "-i" -> "hereComes",
                "-t" -> "1000" //1秒後に答えが出る、そういうJar
                ))))
        }

        //非同期のセット自体が非同期なので、待ち
        timeoutOrDoneOrAfterWait("Workerで非同期なJavaを実行2", worker, dummyParent)

        //エラーになっているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

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
        dummyParent.outputLog
      }
    }
  }

  //Worker processSplit Wait
  if (false) {
    "Worker Order実行　processSplit付き" should {

      "processSplit	Workerにwaitが存在するOrderを渡し、待たせる" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("processSplit	Workerにwaitが存在するOrderを渡し、待たせる")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List(OrderIdentity("WAIT"))),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "jar",
                OrderPrefix._main.toString -> "TestProject",
                OrderPrefix.__delay.toString -> "0",
                "-i" -> "hereComes",
                "-t" -> "1000" //1秒後に答えが出る、そういうJar
                ))))
        }

        //WorkerはSTATUS_SPLIT_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_SPLIT_WAIT)

        val latestWork = worker.getLatestWorkInformation
        dummyParent.outputLog
      }

      "processSplit 待ちが完了するシチュエーション" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("processSplit 待ちが完了するシチュエーション")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List(OrderIdentity("WAIT"))),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        //WorkerはSTATUS_SPLIT_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_SPLIT_WAIT)

        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "WAIT"),
            new TagValue("allfinishedOrderIdentities", List("WAIT"))))

        //WorkerはSTATUS_SPLIT_READYになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_SPLIT_READY)

        //再度同じOrderを送る、今度はprocessSplitにならずに実行されるはず
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List(OrderIdentity("WAIT"))),
              new TagValue("afterWaitIds", List()),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("processSplit 待ちが完了するシチュエーション", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }
    }
  }

  //Order後のwait afterWaitについて
  if (false) {
    "AfterWait" should {

      "waitに入ってからFinishedが来る" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("waitに入ってからFinishedが来る")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List("B")),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("waitに入ってからFinishedが来る", worker, dummyParent)

        //WorkerはSTATUS_AFTER_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)

        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(
            new TagValue("finishedOrderIdentity", "B"),
            new TagValue("allfinishedOrderIdentities", List("B"))))

        timeoutOrDoneOrAfterWait("waitに入ってからFinishedが来る", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }

      "waitに入ってからFinishedが来る　複数のWait" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("waitに入ってからFinishedが来る　複数のWait")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List("B", "C")),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("waitに入ってからFinishedが来る　複数のWait", worker, dummyParent)

        //WorkerはSTATUS_AFTER_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)

        //B
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
            new TagValue("allfinishedOrderIdentities", List("B"))))

        //WorkerはまだSTATUS_AFTER_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)

        //C
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
            new TagValue("allfinishedOrderIdentities", List("B", "C"))))

        timeoutOrDoneOrAfterWait("waitに入ってからFinishedが来る　複数のWait", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず	
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }

      "waitに入る前にFinishedが来る" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("waitに入る前にFinishedが来る")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        //Bが終わった事が伝わる
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
            new TagValue("allfinishedOrderIdentities", List("B"))))

        //この状態で開始
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List("B")),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("waitに入る前にFinishedが来る", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }

      "waitに入る前にFinishedが来る 複数" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("waitに入る前にFinishedが来る 複数")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        //Bが終わった事が伝わる
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
            new TagValue("allfinishedOrderIdentities", List("B"))))

        //Cが終わった事が伝わる
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
            new TagValue("allfinishedOrderIdentities", List("B", "C"))))

        //この状態で開始
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List("B", "C")),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("waitに入る前にFinishedが来る 複数", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }

      "waitに入る前、入った後にFinishedが来る 複数" in {

        //擬似的に親を生成する
        val dummyParent = new DummyWorkerParent("waitに入る前、入った後にFinishedが来る 複数")

        val workerId = UUID.randomUUID().toString
        val worker = new ProcessWorker(workerId, dummyParent.messenger.getName)

        //Bが終わった事が伝わる
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "B"),
            new TagValue("allfinishedOrderIdentities", List("B"))))

        //この状態で開始
        Seq(WorkerMessages.MESSAGE_SETUP.toString, WorkerMessages.MESSAGE_START.toString).foreach { exec =>
          dummyParent.messenger.call(workerId, exec,
            dummyParent.messenger.tagValues(
              new TagValue("identity", "A"),
              new TagValue("processSplitIds", List()),
              new TagValue("afterWaitIds", List("B", "C")),
              new TagValue("context", Map(
                OrderPrefix._kind.toString -> "sh",
                OrderPrefix._main.toString -> "echo some"))))
        }

        timeoutOrDoneOrAfterWait("waitに入る前、入った後にFinishedが来る 複数", worker, dummyParent)

        println("ここですん")
        //WorkerはまだSTATUS_AFTER_WAITに入っているはず
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_AFTER_WAIT)
        println("なの")

        //Cが終わった事が伝わる
        dummyParent.messenger.call(workerId, WorkerMessages.MESSAGE_FINISHEDORDER_NOTIFY.toString,
          dummyParent.messenger.tagValues(new TagValue("finishedOrderIdentity", "C"),
            new TagValue("allfinishedOrderIdentities", List("B", "C"))))

        timeoutOrDoneOrAfterWait("waitに入る前、入った後にFinishedが来る 複数", worker, dummyParent)

        //WorkerはSTATUS_DONEになっているはず		
        worker.currentStatus.head must be_==(WorkerStatus.STATUS_DONE)

        //Aの実行が完了している
        val latestWork = worker.getLatestWorkInformation
        latestWork.localContext.keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        latestWork.localContext(OrderPrefix._result.toString) must be_==("some")
        dummyParent.outputLog
      }
    }
  }
}