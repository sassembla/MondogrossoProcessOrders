package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersControllerTests extends Specification {

	val orderCont = new MondogrossoProcessOrdersController
	
	"パースからマップ取得までのテスト" should {
		
		//construct
		val id = UUID.randomUUID().toString
		val input = "A(else:over:vie else:over:vie)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!Z"
		val json = ""
			
		val parser = new MondogrossoProcessParser(id, input, json)
		val context = parser.parse
		
		
		"attachProcessしたらcontext件数が１増える" in {
			/*
			 * パース結果をorderContにアタッチする
			 */
			val identity = UUID.randomUUID().toString
			val s = orderCont.attachProcess(identity, context)
			
			orderCont.contexts.length == 1 must beTrue
		}
		
		
		"attachしたらContextの内容がセットされる" in {
			val identity = UUID.randomUUID().toString
			val s = orderCont.attachProcess(identity, context)
			
			orderCont.contexts(0).identity.equals(identity) must beTrue
		}
		
		
		"attachされていて	まだ実行されていない	コンディションのContextを実行開始" in {
			orderCont.runAllContext
			
			//実行開始したことによって、Messagingが発生、最初のProcessがStartし、現在のindexが0になっているはず
			orderCont.contexts(0).currentOrderIndex == 0 must beTrue
		}
		
		"その他のコンディションについて" in {
			false == true must beTrue
		}
	}
	
	"Context" should {
		val id = UUID.randomUUID().toString
		val input = "A(else:over:vie else:over:vie)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!Z"
		val json = ""
		
		val parser = new MondogrossoProcessParser(id, input, json)
		val result = parser.parse
	
		val identity = UUID.randomUUID().toString
		val s = orderCont.attachProcess(identity, result)
		
		val currentContext = orderCont.contexts(0)
		
		"Contextを生成した時点で、Context内での値はすべてSequenceとして存在している" in {
			//プロセス数を取得(=で将来作られるWorker数)
			currentContext.processNum == 2 must beTrue
			
			//全体インデックスを取得
			currentContext.totalOrderNum == 6 must beTrue
			
			//現在の進捗インデックスを取得(開始していないので-1)
			currentContext.currentOrderIndex == 0 must beTrue
		}
		
		
		"最初から実行" in {
//			currentContext.start
		}
		
		"途中のindexから実行" in {
//			orderCont.contexts(0).setIndex(1)
		}
	}
	
	"Worker" should {
		"生成されたら、初期値でSTARTEDを親に返すはず" in {
			val masterId = "masterId"
			val childId = "childId"
				
			//この時点でこのcontextはMessengerを持っているはず
			val context = new ProcessContext(masterId, null)
			
			val worker = new context.ProcessWorker(childId, masterId)
			context.run
			
			
		}
	}

	"実行中、コンテキストの値を監視するテスト" should {
		"コンテキストの値を覗き見" in {
			
		}
		
		"未定" in {
			
		}
	}
}