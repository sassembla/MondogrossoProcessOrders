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
		val context = parser.parseProcess
		
		
		"attachProcessしたらcontext件数が１増える" in {
			/*
			 * パース結果をorderContにアタッチする
			 */
			val identity = UUID.randomUUID().toString
			val s = orderCont.attachProcess(identity, context)
			
			orderCont.contexts.length must be_==(1)
		}
		
		
		"attachしたらContextの内容がセットされる" in {
			val identity = UUID.randomUUID().toString
			val s = orderCont.attachProcess(identity, context)
			
			orderCont.contexts(0).identity must be_==(identity)
		}
		
		
		"attachされていて	まだ実行されていない	コンディションのContextを実行開始" in {
			orderCont.runAllContext
			
			//実行開始したことによって、Messagingが発生、最初のProcessがStartし、現在のindexが0になっているはず
			orderCont.contexts(0).currentOrderIndex must be_==(0)
		}
	}
	
	"Context" should {
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
		
		"Contextを生成した時点で、Context内での値はすべてSequenceとして存在している" in {
			//プロセス数を取得(=で将来作られるWorker数)
			currentContext.processNum == 2 must beTrue
			
			//全体インデックスを取得
			currentContext.totalOrderNum == 6 must beTrue
			
			//現在の進捗インデックスを取得
			currentContext.currentOrderIndex must be_==(0) 
		}
		
		
		"最初から実行	現在実行中のOrderがAなハズ" in {
			currentContext.run
			println("currentContext.currentExecutingOrders(0)	"+currentContext.currentExecutingOrders(0))
			currentContext.currentExecutingOrders(0) must be_==("A")
		}
		
		"途中のindexから実行" in {
//			currentContext.startFrom(2)
//			
//			//現在実行中のOrderがAなハズ
//			currentContext.currentExecutingOrders must be_==(Map("A"))
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
			
			"not yet" must be_==("applied")
		}
	}

	"実行中、コンテキストの値を監視するテスト" should {
		"コンテキストの値を覗き見" in {
			
		}
		
		"未定" in {
			
		}
	}
}