package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersControllerTests extends Specification {
	"OrderController" should {
		"attachされていて	まだ実行されていない	コンディションのContextを実行開始" in {
			val orderCont = new MondogrossoProcessOrdersController
			val id = UUID.randomUUID().toString
			val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
			val json = """{	
			"A":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"B":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"C":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"D":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"E":{"type":"sh","class":"AShell.sh","exec":"myExec"}
			}"""

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
		val json = """{
			"A":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"B":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"C":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"D":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"E":{"type":"sh","class":"AShell.sh","exec":"myExec"}
			}"""

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
			currentContext.run
			println("currentContext.currentExecutingOrders(0)	" + currentContext.currentExecutingOrders(0))
			currentContext.currentExecutingOrders(0) must be_==("A")
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
		
		//擬似的に親代わりを生成する
		val dummyParent = new DummyParent()
		
		val id = UUID.randomUUID().toString
		val input = "A>B>C(A:a:c)<E+B>D(A:a2:d1,A:a3:d2)>E!Z"
		val json = """{
			"A":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"B":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"C":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"D":{"type":"sh","class":"AShell.sh","exec":"myExec"},
			"E":{"type":"sh","class":"AShell.sh","exec":"myExec"}
			}"""
		
//		val worker = new ProcessWorker()

		"生成されたら、初期値でSTARTEDを親に返すはず" in {
			"not yet" must be_==("applied")
		}
		
		"typeパラメータを受け取る。種類に応じて" in {
			
		}
		
		 "" in {
			 
		 }
		 
		 "" in {
			 
		 }
	}

}