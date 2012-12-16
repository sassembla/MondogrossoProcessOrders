package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID
import com.kissaki.TagValue
import scala.collection.mutable.ListBuffer
import org.specs2.main.CommandLineArguments
import org.specs2.matcher.TerminationMatchers
import org.specs2.specification.AroundExample
import org.specs2.matcher.MustMatchers
import org.specs2.execute.Result
import org.specs2.time.TimeConversions._


@RunWith(classOf[JUnitRunner])
class MondogrossoContextControllerTests extends Specification {



  /**
  タイムアウト入りの完了ステータスを計る関数
  */
  def timeoutOrDone (title:String, contextCont : MondogrossoContextController, limit:Int = 50) = {
    var i = 0
    while (!contextCont.currentStatus.equals(ContextContStatus.STATUS_EMPTY)) {
      if (limit <= i) {
        sys.error("timeoutOrDone  回数超過 "+contextCont+ "  /title  " + title+"  /このときのstatus  "+contextCont.currentStatus)
        sys.exit(1)
      }
      i+=1
      Thread.sleep(100)
    }
  }

  val dummyProcessOrder = new DummyProcessOrder(UUID.randomUUID.toString)
  val dummyProcessOrderId = dummyProcessOrder.name

  val standardInput = "A>B>C(A:a:c)+(B)D(A:a2:d1,A:a3:d2)!Z"
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
							"_main": "pwd",
							"__finallyTimeout":"2000"
						}
					}"""

  /*
	 * OrderControllerへの単体Contextの投入	
	 */
  if (true) {
    "OrderController" should {

      /*
			 * Contextの実行から終了 
			 */
      "Contextを実行開始、成功結果を受け取る" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val id = UUID.randomUUID().toString
        val input = "A>B>C(A:a:c)<E+(B)D(A:a2:d1,A:a3:d2)>E!Z"

        val parser = new MondogrossoProcessParser(id, input, standardJSON)
        val parseResult = parser.parseProcess

        val identity = UUID.randomUUID().toString
        contextCont.attachProcessOrders(identity, parseResult)

        //起動
        contextCont.runAllContext

        timeoutOrDone("Contextを実行開始、成功結果を受け取る", contextCont)

        //結果がDONE
        contextCont.currentResultsOfContext(identity)(0).status must be_==(ContextStatus.STATUS_DONE)
      }
    }
  }

  /*
	コンテキスト周りの実行テスト
	*/
  if (true) {

    "OrderController コンテキスト実行周りのテスト" should {
      // "Contextを実行開始、各Contextが成功結果を受け取る" in {
      //   val contextCont = new MondogrossoContextController(dummyProcessOrderId)

      //   val id = UUID.randomUUID().toString
      //   val input = "A>B>C(A:a:c)<E+(B)D(A:a2:d1,A:a3:d2)>E!Z"

      //   val parser = new MondogrossoProcessParser(id, input, standardJSON)
      //   val parseResult = parser.parseProcess

      //   val identity = UUID.randomUUID().toString
      //   val s = contextCont.attachProcessOrders(identity, parseResult)

      //   //contextを生成
      //   val currentContext = contextCont.activeContexts(0)

      //   //現在実行中のOrder、内容がまだ無い
      //   contextCont.activeContexts(0).doingOrderIdentities.length must be_==(0)

      //   //起動
      //   contextCont.runAllContext

      //   while (
      //     !currentContext.currentStatus.equals(ContextStatus.STATUS_DONE) &&
      //     !currentContext.currentStatus.equals(ContextStatus.STATUS_TIMEOUTED)
      //     ) {
      //     Thread.sleep(100)
      //     println("Contextを実行開始、各Contextが成功結果を受け取る	" + currentContext.status)
      //   }

      //   currentContext.currentStatus must be_==(ContextStatus.STATUS_DONE)

      //   println("Contextを実行開始、各Contextが成功結果を受け取る	" + contextCont.activeContexts(0).contextKeyValues)
      //   contextCont.activeContexts(0).contextKeyValues.keys must be_==(Set("A", "B", "C", "E", "D", "Z"))

      //   //結果を受け取る
      //   val contextResult = contextCont.activeContexts(0).currentContextResult

      //   //結果は成功
      //   contextResult.status must be_==(ContextStatus.STATUS_DONE)

      //   //コメントを一覧で取得
      //   println("comments	" + contextResult.commentsStack)
      // }

      "Contextを実行開始 Eのロックが解かれないので、達成不可能なオーダー順。　タイムアウトで失敗 結果を受け取る" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val id = UUID.randomUUID().toString
        val input = "A>B>C(A:a:c)<E>E+(B)D(A:a2:d1,A:a3:d2)!Z"

        val parser = new MondogrossoProcessParser(id, input, standardJSON)
        val parseResult = parser.parseProcess

        val identity = UUID.randomUUID().toString
        val s = contextCont.attachProcessOrders(identity, parseResult)

        //起動
        contextCont.runAllContext

        timeoutOrDone("Contextを実行開始 Eのロックが解かれないので、達成不可能なオーダー順。　タイムアウトで失敗 結果を受け取る", contextCont)

        //結果を受け取る
        val contextResult = contextCont.doneContexts(0).currentContextResult

        //結果は失敗
        contextResult.status must be_==(ContextStatus.STATUS_TIMEOUTED)

        //コメントを一覧で取得
        println("timeout	comments	" + contextResult.commentsStack)
      }

      "Contextを実行開始 不正確な値引き継ぎエラーが出る。　エラー結果を受け取る" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val id = UUID.randomUUID().toString
        val input = "A>B>C(A:a:c)+(B)D(Error:a2:d1,A:a3:d2)!Z"

        val parser = new MondogrossoProcessParser(id, input, standardJSON)
        val parseResult = parser.parseProcess

        val identity = UUID.randomUUID().toString
        val s = contextCont.attachProcessOrders(identity, parseResult)

        //contextを生成
        val currentContext = contextCont.activeContexts(0)
        //現在実行中のOrder、内容がまだ無い
        contextCont.activeContexts(0).doingOrderIdentities.length must be_==(0)

        //起動
        contextCont.runAllContext

        while (!currentContext.status.head.equals(ContextStatus.STATUS_ERROR)) {
          Thread.sleep(100)
          println("Contextを実行開始 エラーが出る。　エラー結果を受け取る	" + currentContext.status)
        }

        //結果を受け取る
        val contextResult = contextCont.activeContexts(0).currentContextResult

        //結果は失敗
        contextResult.status must be_==(ContextStatus.STATUS_ERROR)

        //コメントを一覧で取得
        println("error	comments	" + contextResult.commentsStack)
      }
    }
  }

  /*
	 * OrderControllerへの複数Contextの同時投入	
	 */
  if (true) {
    "複数のContextを同時に開始" should {
      "Context C1,C2を同時に開始" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        Seq("C1", "C2").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //2つが準備中
        contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(2)

        //起動
        contextCont.runAllContext

        timeoutOrDone("Context C1,C2を同時に開始", contextCont)

        //2つ終了状態のContextがあるので、状態からContext名が取得できる
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)

        //２つ結果が有る
        contextCont.currentResults.length must be_==(2)
      }
    }
  }

  if (true) {
    "ContextのAttach,Run時に対象Contextのユーザー定義名をセットする/ゲットする" should {
      "attach時、指定した名前と同様の名前を取得" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)
        val C1 = UUID.randomUUID.toString

        val id = UUID.randomUUID().toString
        val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
        val parseResult = parser.parseProcess

        val attachedContextIdentityList = contextCont.attachProcessOrders(C1, parseResult)

        attachedContextIdentityList.contains(C1) must beTrue
      }

      "run時、指定した名前と同様の名前を取得" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)
        val C1 = UUID.randomUUID.toString

        val id = UUID.randomUUID().toString
        val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
        val parseResult = parser.parseProcess

        contextCont.attachProcessOrders(C1, parseResult)
        val startedContextIdentitiesList = contextCont.runAllContext

        startedContextIdentitiesList.contains(C1) must beTrue
      }
    }
  }

  /*
	 * Contextの名前空間の重複を考慮する
	 */
  if (true) {
    "同じ名称のContextを大量に作って実行" should {
      "C1 x ２個" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)
        val C1 = UUID.randomUUID.toString

        Seq(C1, C1).foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //コンテキスト実行
        contextCont.runAllContext

        timeoutOrDone("C1 x ２個", contextCont)

        //2つ終了状態のContextがあるので、状態からContext名が取得できる
        println("C1 x 2個	contexts	"+contextCont.doneContexts)
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)

        //２つ結果が有る
        contextCont.currentResults.length must be_==(2)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }

      "C1 x n個" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)
        val C1 = UUID.randomUUID.toString

        val max = 10

        val C1NSequences = (for (i <- 1 to max) yield C1).toSeq
        C1NSequences.foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //コンテキスト実行
        contextCont.runAllContext

        timeoutOrDone("C1 x n個", contextCont)

        //max個の終了状態のContextがあるので、状態からContext名が取得できる
        println("C1 x n個	contexts	"+contextCont.doneContexts)
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(max)

        //max個の結果が有る
        contextCont.currentResults.length must be_==(max)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }

      "C1,C2 x ２個" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)
        val C1 = UUID.randomUUID.toString
        val C2 = UUID.randomUUID.toString

        Seq(C1, C1, C2, C2).foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //コンテキスト実行
        contextCont.runAllContext

        timeoutOrDone("C1,C2 x ２個", contextCont)

        //2*2つ終了状態のContextがあるので、状態からContext名が取得できる
        println("C1,C2 x ２個	contexts	"+contextCont.doneContexts)
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2 * 2)

        //２*2つ結果が有る
        contextCont.currentResults.length must be_==(2 * 2)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }

      "C1,C2 x n個" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val C1 = UUID.randomUUID.toString
        val C2 = UUID.randomUUID.toString

        val max = 10

        val C1NSequences = (for (i <- 1 to max) yield C1).toSeq
        val C2NSequences = (for (i <- 1 to max) yield C2).toSeq

        val totalSeq = C1NSequences ++ C2NSequences
        totalSeq.foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //コンテキスト実行
        contextCont.runAllContext

        timeoutOrDone("C1,C2 x n個", contextCont)

        //max*2個の終了状態のContextがあるので、状態からContext名が取得できる
        println("C1,C2 x n個	contexts	"+contextCont.doneContexts)
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(max * 2)

        //max*2個の結果が有る
        contextCont.currentResults.length must be_==(max * 2)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }
    }
  }

  /**
   * タイムアウトになったContextがある場合も、ContextControllerはEmptyになる。
   */
  if (true) {
    "タイムアウトになったContextがある場合も、ContextControllerはEmptyになる" should {
      "タイムアウト" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val id = UUID.randomUUID().toString
        val input = "A>B>C(A:a:c)<E>E+(B)D(A:a2:d1,A:a3:d2)!Z" //実行不可能、タイムアウトになる

        val parser = new MondogrossoProcessParser(id, input, standardJSON)
        val parseResult = parser.parseProcess

        val identity = UUID.randomUUID().toString
        contextCont.attachProcessOrders(identity, parseResult)

        //起動
        contextCont.runAllContext

        timeoutOrDone("タイムアウト", contextCont)

        //結果を受け取る
        val contextResult = contextCont.doneContexts(0).currentContextResult

        //結果はタイムアウト
        contextResult.status must be_==(ContextStatus.STATUS_TIMEOUTED)
      }
    }
  }

  /*
	 * OrderControllerへの複数Contextの順次投入
	 */
  if (true) {
    "複数のContextを順次導入" should {
      "C1投入後、C2を投入" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        Seq("C1").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //1つが準備中
        contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)

        //起動
        contextCont.runAllContext

        Seq("C2").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        // //1つが準備中、一つが実行中(どのstateかは不明)
        contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)

        //起動2
        contextCont.runAllContext

        //待つ
        timeoutOrDone("C1投入後、C2を投入", contextCont)

        //2つ終了状態のContextがあるので、状態からContext名が取得できる
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)

        //２つ結果が有る
        contextCont.currentResults.length must be_==(2)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }

      "C1投入後、完了を待ってC2を投入" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        Seq("C1").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //1つが準備中
        contextCont.contextCountOfStatus(ContextStatus.STATUS_READY.toString).length must be_==(1)

        //起動
        contextCont.runAllContext

        //待つ1
        timeoutOrDone("C1投入後、完了を待ってC2を投入-1", contextCont)

        Seq("C2").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //起動2
        contextCont.runAllContext

        //待つ2
        timeoutOrDone("C1投入後、完了を待ってC2を投入-2", contextCont)

        //2つ終了状態のContextがあるので、状態からContext名が取得できる
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(2)

        //２つ結果が有る
        contextCont.currentResults.length must be_==(2)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }

      "C1投入後/実行、C2を投入後/実行、C3を投入" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        Seq("C1").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //起動1
        contextCont.runAllContext

        Seq("C2").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //起動2
        contextCont.runAllContext

        Seq("C3").foreach { contextIdentity =>
          val id = UUID.randomUUID().toString

          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //起動3
        contextCont.runAllContext

        //待つ
        timeoutOrDone("C1投入後/実行、C2を投入後/実行、C3を投入", contextCont)

        //3つ終了状態のContextがあるので、状態からContext名が取得できる
        contextCont.contextCountOfStatus(ContextStatus.STATUS_DONE.toString).length must be_==(3)

        //3つ結果が有る
        contextCont.currentResults.length must be_==(3)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }
    }
  }

  if (false) {
    "大量のProcessOrderを同時に" should {
      "100件" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val max = 100

        val processOrderSequences = (for (i <- 1 to max) yield UUID.randomUUID.toString).toSeq
        processOrderSequences.foreach { contextIdentity =>
          val id = UUID.randomUUID().toString
          val parser = new MondogrossoProcessParser(id, standardInput, standardJSON)
          val parseResult = parser.parseProcess

          contextCont.attachProcessOrders(contextIdentity, parseResult)
        }

        //起動
        contextCont.runAllContext

        //待つ
        timeoutOrDone("大量のProcessOrderを同時に", contextCont)

				//3つ結果が有る
        contextCont.currentResults.length must be_==(100)

        //結果を受け取る
        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet

        //結果はすべてDone
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }
    }
  }

  /*
    Safariの起動とテスト結果の表示
  */
  if (false) {
    "report" should {
      "report run" in {
        val contextCont = new MondogrossoContextController(dummyProcessOrderId)

        val JSON = """
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
							"_main": "open /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/index.html",
							"__finallyTimeout":"2000"
						}
					}"""
        val contextIdentity = UUID.randomUUID().toString
        val id = UUID.randomUUID().toString

        val parser = new MondogrossoProcessParser(id, standardInput, JSON)
        val parseResult = parser.parseProcess

        contextCont.attachProcessOrders(contextIdentity, parseResult)

        contextCont.runAllContext

        //待つ
        timeoutOrDone("report run", contextCont)

        val contextResultSet = (for (doneCont <- contextCont.doneContexts) yield doneCont.currentContextResult).toSet
        contextResultSet.forall(_.status == ContextStatus.STATUS_DONE) must beTrue
      }
    }
  }


}