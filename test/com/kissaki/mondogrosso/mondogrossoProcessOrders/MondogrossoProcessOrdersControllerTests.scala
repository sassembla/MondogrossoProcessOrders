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
	val standardJSON = """{"A": {"_kind": "sh","_main": "AShell.sh","key": "value","key2": "value2"}}"""

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

	"Context" should {
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

	"Worker" should {
		val TEST_PROCESS_1 = "TEST_PROCESS_1"
		val TEST_PROCESS_2 = "TEST_PROCESS_2"
		val TEST_PROCESS_3 = "TEST_PROCESS_3"

		//擬似的に親代わりを生成する
		val dummyParent = new DummyParent()

		val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
		val json = standardJSON

		//Workerは、担当するProcessを与えられる

		"Workerは現在実行中のInformationを所持しているはず" in {
			val worker = new ProcessWorker(TEST_PROCESS_1, dummyParent.messenger.getName)
			dummyParent.messenger.call(TEST_PROCESS_1, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"b" -> "c"))))

			worker.workerIdentity must be_==(TEST_PROCESS_1)

			//Masterから与えられて変更になる、即時的な箇所
			val latestWork = worker.getLatestWorkInformation

			latestWork.orderIdentity must be_==("A")
			latestWork.context must be_==(Map(
				"_kind" -> "sh",
				"_main" -> "ls -l",
				"b" -> "c"))
		}

		"Workerを完全同期で実行後、実行完了したのでDone状態" in {
			val worker = new ProcessWorker(TEST_PROCESS_2, dummyParent.messenger.getName)
			dummyParent.messenger.call(TEST_PROCESS_2, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"key" -> "value"))))

			//実行が同期的に行われ、実行されたあとの情報が残る
			worker.currentStatus must be_==(WorkerStatus.STATUS_DONE)
		}

		"Workerを実行、実行後のContext確認" in {
			val worker = new ProcessWorker(TEST_PROCESS_3, dummyParent.messenger.getName)
			dummyParent.messenger.call(TEST_PROCESS_3, Messages.MESSAGE_START.toString,
				dummyParent.messenger.tagValues(
					new TagValue("identity", "A"),
					new TagValue("context", Map(
						"_kind" -> "sh",
						"_main" -> "ls -l",
						"key" -> "value"))))

			//結果が残っているはず
			"not yet applied" must be_==("")
		}
	}

}