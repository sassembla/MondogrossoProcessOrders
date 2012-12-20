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
class MondogrossoProcessContextTests extends Specification {

  /**
   * タイムアウト入りの完了ステータスを計る関数
   */
  def timeoutOrDone(identity: String, currentContext: MondogrossoProcessContext, limit: Int = 100) = {
    var i = 0
    while (!currentContext.status.head.equals(ContextStatus.STATUS_DONE) && 
      !currentContext.status.head.equals(ContextStatus.STATUS_TIMEOUTED) && 
      !currentContext.status.head.equals(ContextStatus.STATUS_ERROR)
      ) {
      if (limit <= i) {
        sys.error(identity + "  回数超過 " + currentContext)
        sys.exit(1)
      }

      println(identity + "  /回数 " + i + " /limit " + limit+"  /ステータスは "+currentContext.status)
      i += 1
      Thread.sleep(100)

    }
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
								"_main": "pwd"
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
								"_main": "echo a"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd"
							}
						}
					"""

  //Context information
  if (false) {
    "Context Setting" should {
      "開始前のセット結果" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)

        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
  						{"A": 
  							{
  								"_kind": "sh",
  								"_main": "pwd"
  							},
  						"Z": 
  							{
  								"_kind": "sh",
  								"_main": "pwd",
  								"__finallyTimeout":"100"
  							}
  						}
  					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "開始前のセット結果"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //run前、ContextのstatusがREADY
        currentContext.status.head must be_==(ContextStatus.STATUS_READY)
      }
    }

  }

  //Context
  if (false) {
    "Context run" should {
      "Context 基礎的な挙動　run A then Z" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "Context 基礎的な挙動　run A then Z" //UUID.randomUUID().toString
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        timeoutOrDone(identity, currentContext)

        //実行履歴
        currentContext.status must be_==(ListBuffer(ContextStatus.STATUS_DONE,
          ContextStatus.STATUS_FINALLY,
          ContextStatus.STATUS_RUNNING,
          ContextStatus.STATUS_READY,
          ContextStatus.STATUS_NOTREADY))

        //Aの実行記録がある
        currentContext.contextKeyValues.get("A").getOrElse(Map("key" -> "value")).keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))

        //Zの実行記録がある
        currentContext.contextKeyValues.get("Z").getOrElse(Map("key" -> "value")).keys must be_==(Set(
          OrderPrefix._kind.toString,
          OrderPrefix._main.toString,
          OrderPrefix._result.toString))
      }
    }
  }

  //timeouts
  if (false) {
    "Context タイムアウトについて" should {
      "1:run A then Z finallyTimeout付きで時間内に完了する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"8000"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "1:run A then Z finallyTimeout付きで時間内に完了する" //UUID.randomUUID().toString
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        timeoutOrDone(identity, currentContext)

        //実行履歴
        currentContext.status must be_==(ListBuffer(ContextStatus.STATUS_DONE,
          ContextStatus.STATUS_FINALLY,
          ContextStatus.STATUS_RUNNING,
          ContextStatus.STATUS_READY,
          ContextStatus.STATUS_NOTREADY))
      }

      "2:run A then Z 1msec で　ContextTimeoutする" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "このJarが実行されたのは3:run A then Z 1msec で　ContextTimeoutする です",
								"-t" : "1000"
								
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

        val identity = "2:run A then Z 1msec で　ContextTimeoutする" //UUID.randomUUID().toString
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //コンテキストからの実行開始
        timeoutOrDone(identity, currentContext)


        //実行履歴
        currentContext.status must be_==(ListBuffer(ContextStatus.STATUS_TIMEOUTED,
          ContextStatus.STATUS_TIMEOUT,
          ContextStatus.STATUS_RUNNING,
          ContextStatus.STATUS_READY,
          ContextStatus.STATUS_NOTREADY))
      }

      "3:run A then Z 0msec で正常終了" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "3:run A then Z 0msec で正常終了" //UUID.randomUUID().toString
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext


        //Done待ち
        timeoutOrDone(identity, currentContext)

        //実行履歴
        currentContext.status must be_==(ListBuffer(ContextStatus.STATUS_DONE,
          ContextStatus.STATUS_FINALLY,
          ContextStatus.STATUS_RUNNING,
          ContextStatus.STATUS_READY,
          ContextStatus.STATUS_NOTREADY))
      }
    }
  }

  //split
 if (false) {
    /*
		 * 分裂：		A+(A)B+(A)C+(A)D					複数のプロセスが同時に並列で開始
		 * 連鎖分裂：	A+(A)B+(B)C+(C)D					連鎖的に並列で開始
		 * 収束:		A>E<B,C,D+(A)B+(B)C+(C)D!Z			B,C,Dが完了したらやっとEが終わる
		 */
    "processSplitの分裂、連鎖分裂、収束" should {
      "processSplitが2連続する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A+(A)B!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "processSplitが2連続する"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)
        //A,B,Zともに終了している
        println("processSplitが2連続する	currentContext.contextKeyValues	" + currentContext.contextKeyValues)

        /*
    		 * processSplitが2連続する	currentContext.contextKeyValues	
    		 * Map(Z -> Map(__finallyTimeout -> 0, _result -> /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders, _kind -> sh, _main -> pwd), 
    		 * A -> Map(_result -> /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders, _kind -> sh, _main -> pwd), 
    		 * B -> Map(_kind -> sh, _main -> pwd))	/i	0
    		 */
        Seq("A", "B").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }

      "processSplitが3分裂する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A+(A)B+(A)C!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""
        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "processSplitが3分裂する"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,Zともに終了している
        println("processSplitが3分裂する	currentContext.contextKeyValues	" + currentContext.contextKeyValues)
        Seq("A", "B", "C").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }
  
      "複雑な分裂" in {

        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "複雑A+(複雑A)複雑B+(複雑A)複雑C+(複雑B)複雑D+(複雑B)複雑E!複雑Z"
        val json = """

						{"複雑A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑D": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑E": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""


        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "複雑な分裂"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,D,E,Zともに終了している
        println("processSplitが3連鎖分裂するcurrentContext.contextKeyValues	" + currentContext.contextKeyValues)

        Seq("複雑A", "複雑B", "複雑C", "複雑D", "複雑E").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("複雑Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }

      "収束 A,Bが発生、AからCが発生、B終了に合わせてZへ" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "収束A>収束C<収束B+(収束A)収束B!収束Z"
        val json = """
						{"収束A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"収束B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"収束C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"収束Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "収束 A,Bが発生、AからCが発生、B終了に合わせてZへ"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)


        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,Zともに終了している
        println("processSplitが3連鎖分裂するcurrentContext.contextKeyValues	" + currentContext.contextKeyValues)

        Seq("収束A", "収束B", "収束C").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("収束Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }

      "収束 A,Bが発生、Bに時間のかかる処理、Cが発生、終了かつB待ち、B終了、Z" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>C<B+(A)B!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "B",
								"-t" : "100"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "収束 A,Bが発生、Bに時間のかかる処理、Cが発生、終了かつB待ち、B終了、Z"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,Zともに終了している

        Seq("A", "C").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("B").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            "-t",
            "-i",
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }

      "複雑な収束 A,B,D,E,C,Fで、F時にロックが解けているので、Zへ" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>F<B,C,D,E+(A)B+(A)C+(B)D+(B)E!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"D": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"E": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"F": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}

					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "複雑な収束 A,B,D,E,C,Fで、F時にロックが解けているので、Zへ"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,D,E,F,Zともに終了している
        Seq("A", "B", "C", "D", "E", "F").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }



      "複雑な収束 A,C,F,B,D,E,で、E完了時にロックが解けているので、Zへ" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "複雑な収束A>複雑な収束F<複雑な収束B,複雑な収束C,複雑な収束D,複雑な収束E+(複雑な収束A)複雑な収束B+(複雑な収束A)複雑な収束C+(複雑な収束B)複雑な収束D+(複雑な収束B)複雑な収束E!複雑な収束Z"
        val json = """
						{"複雑な収束A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑な収束B": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "B",
								"-t" : "100"
							},
						"複雑な収束C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑な収束D": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "D",
								"-t" : "1000"
							},
						"複雑な収束E": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "E",
								"-t" : "1000"
							},
						"複雑な収束F": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"複雑な収束Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "複雑な収束 A,C,F,B,D,E,で、E完了時にロックが解けているので、Zへ"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)
        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //A,B,C,D,E,F,Zともに終了している
        Seq("複雑な収束A", "複雑な収束C", "複雑な収束F").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("複雑な収束B", "複雑な収束D", "複雑な収束E").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix._kind.toString,
            "-t",
            "-i",
            OrderPrefix._main.toString,
            OrderPrefix._result.toString))
        }

        Seq("複雑な収束Z").foreach { orderIdentity =>
          currentContext.contextKeyValues.apply(orderIdentity).keys must be_==(Set(
            OrderPrefix.__finallyTimeout.toString,
            OrderPrefix._result.toString,
            OrderPrefix._kind.toString,
            OrderPrefix._main.toString))
        }
      }
    }
  }

  //special cases
  if (false) {
    "Context 複雑なOrder" should {
      "5:run A,B,Z 複数のOrder" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>B!Z" //なんでもgrep
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
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

        val identity = "5:run A,B,Z 複数のOrder"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //実行順が入っているはず
        println("5:run A,B,Z 複数のOrder	doneOrderIdentities	" + currentContext.doneOrderIdentities)
        currentContext.doneOrderIdentities must be_==(List("A", "B", "Z"))
      }

      "6.0:run A,B(A:_result:in),Z 複数のOrderで値を適応、一つの適応値発生を確認する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>B(A:_result:1stPlace)!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo FirstParamReplaced"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "echo"
								"1stPlace" : "should be replace to 1st"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"1000"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "6.0:run A,B(A:_result:in),Z 複数のOrderで値を適応、一つの適応値発生を確認する"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("6.0	" +identity+ currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //BがAの_resultをgrepした結果を持つ
        currentContext.contextKeyValues.get("B").get("1stPlace") must be_==("FirstParamReplaced")
      }

      "6.01:run A,B(A:_result:in),Z 複数のOrderで値を適応、複数の値の適応値発生を確認する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>B(A:a1:1stPlace, A:a2:2ndPlace)!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo FirstParamReplaced",
									"a1": "A1Param",
									"a2": "A2Param"
				
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "echo"
								"1stPlace" : "should be replace to 1st",
								"2ndPlace" : "should be replace to 2nd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"1000"
							}
						}
					"""


        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "6.01:run A,B(A:_result:in),Z 複数のOrderで値を適応、複数の値の適応値発生を確認する"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("6.01	" + currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //BがAの_resultをgrepした結果を持つ
        currentContext.contextKeyValues.get("B").get("1stPlace") must be_==("A1Param")
        currentContext.contextKeyValues.get("B").get("2ndPlace") must be_==("A2Param")
      }

      "6.1:run A,B(A:_result:in),Z 複数のOrderで値を適応オリジナル" in {


        val contextParent = new DummyContextParent(UUID.randomUUID.toString)

        val id = UUID.randomUUID().toString
        val input = "A>B(A:_result:a)!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo \"build.gradle\""
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"a" : "should be grep",
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"1000"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess
        val identity = "6.1:run A,B(A:_result:in),Z 複数のOrderで値を適応オリジナル"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)
        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("6.1	" + currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //実行順が入っているはず
        println("6.1:run A,B(A:_result:in),Z 複数のOrderで値を適応	doneOrderIdentities	" + currentContext.doneOrderIdentities)
        currentContext.doneOrderIdentities must be_==(List("A", "B", "Z"))
      }

      "6.5:複数のOrderで値を共有する" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>B(A:_result:b)>C(A:_result:-e)!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep",
								"b" : "should be grep of A's result"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "echo",
								"-e" : "should be address"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "open",
								"-a":"Safari.app /Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersControllerTests.html"
							}
						}
					"""


        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "6.5:複数のOrderで値を共有する"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("6.5	" + currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)


        //実行順が入っているはず
        println("doneOrderIdentities	" + currentContext.doneOrderIdentities)
        currentContext.doneOrderIdentities must be_==(List("A", "B", "C", "Z"))

        //各処理の結果が入っているはず

        //代入された値が確認できる　aの_result は、 bのbへと代入されているはず。
        val aResult = currentContext.contextKeyValues.get("A").get("_result")
        val bB = currentContext.contextKeyValues.get("B").get("b")
        aResult must be_==(bB)

        //代入された値が確認できる　aの_result は、 bのbへと代入されているはず。
        val c_e = currentContext.contextKeyValues.get("C").get("-e")
        aResult must be_==(c_e)
      }

      "7:時間のかかる処理を並列で行う" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "B7+(B7)C+(B7)D+(B7)E!Z"
        val json = """
						{
						"B7": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7B",
								"-t" : "1000"
								
							},
						"C": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Cです",
								"-t" : "1000"
								
							},
						"D": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Dです",
								"-t" : "1000"
								
							},
						"E": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "7Eです",
								"-t" : "1000"
								
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "7:時間のかかる処理を並列で行う"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //完了待ち
        timeoutOrDone(identity, currentContext)

        //実行順が入っているはず 途中の順番は不明
        println("7	doneOrderIdentities	" + currentContext.doneOrderIdentities)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        currentContext.doneOrderIdentities.toSet must be_==(Set("B7", "D", "E", "C", "Z"))
      }


      "7.1:時間のかかる処理を並列で行う、タイムアウト付き" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "B7.1+(B7.1)C7.1+(B7.1)D7.1+(B7.1)E7.1!Z7.1"
        val json = """
            {
            "B7.1": 
              {
                "_kind": "jar",
                "_main": "TestProject",
                "-i" : "7.1",
                "-t" : "1000",
                "__timeout":"100"
              },
            "C7.1": 
              {
                "_kind": "jar",
                "_main": "TestProject",
                "-i" : "7.1",
                "-t" : "1000",
                "__timeout":"100"
                
              },
            "D7.1": 
              {
                "_kind": "jar",
                "_main": "TestProject",
                "-i" : "7.1",
                "-t" : "1000",
                "__timeout":"100"
                
              },
            "E7.1": 
              {
                "_kind": "jar",
                "_main": "TestProject",
                "-i" : "7.1",
                "-t" : "1000",
                "__timeout":"100"
                
              },
            "Z7.1": 
              {
                "_kind": "sh",
                "_main": "pwd",
                "__finallyTimeout":"1000"
              }
            }
          """

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "7.1:時間のかかる処理を並列で行う タイムアウト付き"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //  Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        //実行順が入っているはず 途中の順番は不明
        println("7.1  doneOrderIdentities " + currentContext.doneOrderIdentities)

        currentContext.status.head must be_==(ContextStatus.STATUS_TIMEOUTED)
      }

     
      "8:待ちが存在するOrder Aが完了したらBが動き出す" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A+(A)B(A:_result:\"a\")!Z"
        val json = """
 						{"A": 
							{
								"_kind": "sh",
								"_main": "echo ./*.*"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"\"a\"" : "should be grep"
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

        val identity = "8:待ちが存在するOrder Aが完了したらBが動き出す"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("8	" + currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //実行順が入っているはず
        println("8:	doneOrderIdentities	" + currentContext.doneOrderIdentities)
        currentContext.doneOrderIdentities must be_==(List("A", "B", "Z"))

        //BがAの_resultをgrepした結果を持つ
        println("8:currentContext	" + currentContext.contextKeyValues)
        currentContext.contextKeyValues.apply("B").apply("\"a\"") must be_==("./*.*")
      }

      "8.1:待ちが存在するOrder Aが完了したらBが動き出す 要素に\"\"が含まれる" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A+(A)B(A:_result:\"a\")!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo \"./*.*\""
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"\"a\"" : "should be grep"
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
        val identity = "8.1:待ちが存在するOrder Aが完了したらBが動き出す 要素に\"\"が含まれる"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)
        currentContext.runContext
        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("8.1	" + currentContext.currentContextResult.commentsStack)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //実行順が入っているはず
        currentContext.doneOrderIdentities must be_==(List("A", "B", "Z"))
        //BがAの_resultをgrepした結果を持つ
        println("8.1:currentContext	" + currentContext.contextKeyValues)
      }
      
      "9:待ちが存在するOrder Aの値を継いで、Aが完了したらBが動き出す" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A+(A)B(A:_result:gradle)!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "echo \"build.gradle\""
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "grep"
								"gradle" : "should be grep"
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
        val identity = "9:待ちが存在するOrder Aの値を継いで、Aが完了したらBが動き出す"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)
        currentContext.runContext
        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        println("9	" + currentContext.currentContextResult.commentsStack)
        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)

        //実行順が入っているはず
        println("9	doneOrderIdentities	" + currentContext.doneOrderIdentities)
        currentContext.doneOrderIdentities must be_==(List("A", "B", "Z"))
        //BがAの_resultをgrepした結果を持つ
        println("9:currentContext	" + currentContext.contextKeyValues)
        // 				currentContext.contextKeyValues("B").apply("in") != null must beTrue
      }

    }
  }

  //Context Error
  if (false) {
    "Context エラー処理" should {


      "__finallyTimeout設定によるタイムアウト発生" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "cat build.gradle"
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

        val identity = "__finallyTimeout設定によるタイムアウト発生"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        //	Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        //タイムアウトで終わったら、その結果を明示
        val contextResult = currentContext.currentContextResult
        contextResult.status must be_==(ContextStatus.STATUS_TIMEOUTED)
      }

      "実行前エラー __finallyTimeoutの設定ミス" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "something wrong"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"-1"
							}
						}

					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess
        val identity = "実行前エラー __finallyTimeoutの設定ミス"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext
        timeoutOrDone(identity, currentContext)


        //エラーで終わったら、その結果を明示
        val contextResult = currentContext.currentContextResult
        contextResult.status must be_==(ContextStatus.STATUS_ERROR)
      }


      "実行時エラー 誤ったshell内容" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "something wrong"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)

        val result = parser.parseProcess

        val identity = "実行時エラー 誤ったshell内容"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        currentContext.runContext

        timeoutOrDone(identity, currentContext)

        //エラーで終わったら、その結果を明示
        val contextResult = currentContext.currentContextResult
        contextResult.status must be_==(ContextStatus.STATUS_ERROR)
      }

    }
  }

   if (false) {
    "よく停止しているように見受けるケース　additionalTestCase" should {
      
      "解消されないテスト p1 Bの実行に1Sかかり、必ずCのafterWaitがセットされてからになる" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "よく停止しているA>よく停止しているC<よく停止しているB+(よく停止しているA)よく停止しているB!よく停止しているZ"
        val json = """
						{"よく停止しているA": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"よく停止しているB": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "B",
								"-t" : "1000"
							},
						"よく停止しているC": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"よく停止しているZ": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "解消されないテスト p1 Bの実行に1Sかかり、必ずCのafterWaitがセットされてからになる"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)
      }

      "解消されないテスト p2 Cの実行に1Sかかり、必ずCのafterWaitがセットされる前にBが終わる" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>C<B+(A)B!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"C": 
							{
								"_kind": "jar",
								"_main": "TestProject",
								"-i" : "C",
								"-t" : "1000"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "解消されないテスト p2 Cの実行に1Sかかり、必ずCのafterWaitがセットされる前にBが終わる"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)
      }

      "解消されないテスト p0 確率的に失敗するっぽいデフォルト" in {
        val contextParent = new DummyContextParent(UUID.randomUUID.toString)
        val id = UUID.randomUUID().toString
        val input = "A>C<B+(A)B!Z"
        val json = """
						{"A": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"B": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"C": 
							{
								"_kind": "sh",
								"_main": "pwd"
							},
						"Z": 
							{
								"_kind": "sh",
								"_main": "pwd",
								"__finallyTimeout":"0"
							}
						}
					"""

        val parser = new MondogrossoProcessParser(id, input, json)
        val result = parser.parseProcess

        val identity = "解消されないテスト p0 確率的に失敗するっぽいデフォルト"
        val currentContext = new MondogrossoProcessContext(identity, result, contextParent.messenger.getName)

        //コンテキストからの実行開始
        currentContext.runContext

        //Timeout処理の待ち
        timeoutOrDone(identity, currentContext)

        currentContext.status.head must be_==(ContextStatus.STATUS_DONE)
      }
    }
  }
}