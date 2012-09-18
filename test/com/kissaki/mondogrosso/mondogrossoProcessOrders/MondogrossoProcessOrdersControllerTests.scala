package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersControllerTests extends Specification {

	val runner = new MondogrossoProcessOrdersController
	
	"パースからマップ取得までのテスト" should {
		val id = UUID.randomUUID().toString
		val input = "A(else:over:vie else:over:vie)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!Z"
		val json = ""
			
		val parser = new MondogrossoProcessParser(id, input, json)
		val result = parser.parse
		
		"attachProcessしたらcontext件数が１増える" in {
			/*
			 * パース結果をrunnerにアタッチする
			 */
			val identity = UUID.randomUUID().toString
			val s = runner.attachProcess(identity, result)
			
			runner.contexts.length == 1 must beTrue
		}
		
		"attachしたらContextの内容がセットされる" in {
			val identity = UUID.randomUUID().toString
			val s = runner.attachProcess(identity, result)
			
			runner.contexts(0).identity.equals(identity) must beTrue
		}
		
		"attachされていてまだ実行されていないContextを実行開始" in {
			runner.runAllContext
		}
	}

	"runner.startするところまでのテスト" should {
		val id = UUID.randomUUID().toString
		val input = "A(else:over:vie else:over:vie)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!Z"
		val json = ""
		
		val parser = new MondogrossoProcessParser(id, input, json)
		val result = parser.parse
	
		val identity = UUID.randomUUID().toString
		val s = runner.attachProcess(identity, result)
		
		val currentContext = runner.contexts(0)
		
		"Contextを生成した時点で、Context内での値はすべてSequenceとして存在している" in {
			//プロセス数を取得(=で将来作られるWorker数)
			currentContext.processNum == 2 must beTrue
			
			//全体インデックスを取得
			currentContext.totalOrderNum == 6 must beTrue
			
			//現在の進捗インデックスを取得(開始していないので-1)
			currentContext.orderIndex == -1 must beTrue
		}
		
		
		"最初から実行" in {
//			currentContext.start
		}
		
		"途中のindexから実行" in {
//			runner.contexts(0).setIndex(1)
		}
		
	}

	"実行中、コンテキストの値を監視するテスト" should {
		"コンテキストの値を覗き見" in {
			
		}
		
		"未定" in {
			
		}
	}
}