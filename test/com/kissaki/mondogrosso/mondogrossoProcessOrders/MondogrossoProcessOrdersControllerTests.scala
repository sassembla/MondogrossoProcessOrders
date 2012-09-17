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
	
	/*
	 * Orderの順と合致をクラスにし終わったので、そこから処理を実行するclassを書き上げる。まだ先が長い！　が、良い事ある。
	 * 最小限を組んで動かせるようにしよう。
	 * 
	 * ★Contextの扱い
	 * 実行時、すべての基礎になるもの。
	 * どれかのWorkerからdoneが来たら、内包してるものと見比べて
	 * Workerに値を渡す。
	 * 
	 * errorを受け取ったら、finallyを呼ぶ
	 * timeoutを受け取ったら、finallyを呼ぶ
	 * 
	 * 
	 * ★finallyの扱い
	 * 最後まで走ったら、finallyを実行
	 * どこかでタイムアウトしてもエラーがでても、finallyを実行
	 * 
	 * ★Orderの扱い
	 * 一つのOrderは、start,running,done,error,timeoutの5つの状態を持つ。
	 * OrdersごとのWorkerで実行される。
	 * 
	 * >	start時にContextから値を受け取る
	 * -	running時は何もしない
	 * <	done時はContextに値を渡す
	 * <	error時はエラーをContextに渡す
	 * <	timeout時はタイムアウトイベントをContextに渡す
	 * 
	 * 
	 * ★waitの扱い
	 * waitは、到達したら、Contextをチェック、存在すれば次のOrderへ。
	 * 存在しなければ、待機する。
	 * 
	 * 
	 * ★+の扱い
	 * +で追加される、待ち状態から開始されるOrders
	 * 最初のOrderが、待ちOrderの予約になる。Mapからクラスを作る際に、secondary以降であれば
	 * Parallelとして扱う。
	 * 
	 * 同じContext内の同名のOrderが消えた瞬間から並列でRunする。
	 */

	

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