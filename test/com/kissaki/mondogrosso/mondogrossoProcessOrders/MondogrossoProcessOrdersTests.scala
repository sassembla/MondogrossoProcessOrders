package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {

  val standardInput = "A!Z"
  val standardJSON = """
		{"A": 
			{
				"_kind": "sh",
				"_main": "echo",
				"a":"something",
				"a2":"else",
				"a3":"other",
			},
		"Z": 
			{
				"_kind": "sh",
				"_main": "pwd",
				"__finallyTimeout":"2000"
			}
		}
	"""

  

  if (true) {
    "-pと-sが有れば、入力をプロセス自体として受けて、プロセスを実行する" should {
      "A!Z" in {
      	val input = Array(//コレが原因っぽい
		  	"-i", "AtoZ",
		    "-p", standardInput,
		    "-s", standardJSON,
		    "-o", ""
		)
		/*

Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.474 [DEBUG] [TestEventLogger]     com.kissaki.MessengerActor$SubMessengerActor@4c623b03: caught java.lang.IndexOutOfBoundsException: 0
03:05:59.474 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.475 [DEBUG] [TestEventLogger]     java.lang.IndexOutOfBoundsException: 0
03:05:59.475 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.475 [DEBUG] [TestEventLogger]     	at scala.collection.mutable.ListBuffer.apply(ListBuffer.scala:72)
03:05:59.475 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.475 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor.callParent(Messenger.scala:393)
03:05:59.476 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.476 [DEBUG] [TestEventLogger]     	at com.kissaki.Messenger.callParent(Messenger.scala:177)
03:05:59.476 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.476 [DEBUG] [TestEventLogger]     	at com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoContextController.receiver(MondogrossoContextController.scala:79)
03:05:59.476 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.477 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor$SubMessengerActor$$anonfun$act$3$$anonfun$apply$5.apply(Messenger.scala:532)
03:05:59.477 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.477 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor$SubMessengerActor$$anonfun$act$3$$anonfun$apply$5.apply(Messenger.scala:491)
03:05:59.477 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.477 [DEBUG] [TestEventLogger]     	at scala.actors.ReactorTask.run(ReactorTask.scala:31)
03:05:59.478 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.478 [DEBUG] [TestEventLogger]     	at scala.actors.Reactor$class.resumeReceiver(Reactor.scala:129)
03:05:59.478 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.478 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor$SubMessengerActor.scala$actors$ReplyReactor$$super$resumeReceiver(Messenger.scala:486)
03:05:59.478 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.478 [DEBUG] [TestEventLogger]     	at scala.actors.ReplyReactor$class.resumeReceiver(ReplyReactor.scala:68)
03:05:59.479 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.479 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor$SubMessengerActor.resumeReceiver(Messenger.scala:486)
03:05:59.479 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.479 [DEBUG] [TestEventLogger]     	at scala.actors.Actor$class.searchMailbox(Actor.scala:500)
03:05:59.479 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.479 [DEBUG] [TestEventLogger]     	at com.kissaki.MessengerActor$SubMessengerActor.searchMailbox(Messenger.scala:486)
03:05:59.480 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.480 [DEBUG] [TestEventLogger]     	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply$mcV$sp(Reactor.scala:117)
03:05:59.480 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.480 [DEBUG] [TestEventLogger]     	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply(Reactor.scala:114)
03:05:59.480 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.480 [DEBUG] [TestEventLogger]     	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply(Reactor.scala:114)
03:05:59.481 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.481 [DEBUG] [TestEventLogger]     	at scala.actors.ReactorTask.run(ReactorTask.scala:33)
03:05:59.481 [DEBUG] [org.gradle.messaging.remote.internal.WorkerProtocol] Dispatching request to worker: [Request consumer: c2aaedc7-416e-4e73-8c58-c01cbf39fdba, payload: [MethodInvocation method: output()]]
03:05:59.481 [DEBUG] [TestEventLogger]     	at scala.concurrent.forkjoin.ForkJoinPool$AdaptedRunnable.exec(ForkJoinPool.java:611)
03:05:59.481 [DEBUG] [TestEventLogger]     	at scala.concurrent.forkjoin.ForkJoinTask.quietlyExec(ForkJoinTask.java:422)
03:05:59.482 [DEBUG] [TestEventLogger]     	at scala.concurrent.forkjoin.ForkJoinWorkerThread.mainLoop(ForkJoinWorkerThread.java:340)
03:05:59.482 [DEBUG] [TestEventLogger]     	at scala.concurrent.forkjoin.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:325)
> Building > :test > 32 tests completed, 1 failed

		*/
        val result = MondogrossoProcessOrders.main(input)

        println("result	" + result)
        "not yet implement" must be_==("")
      }
    }
  }

  if (false) {
    "replモード" should {
      "-p,-s無し" in {
        //やるとしたら、自動的に入力、みたいなこともしないといけない。　要件を纏めよう。このモード危険な気がする。
        "not yet implement" must be_==("")
      }

      "-p,-s有り" in {
        "not yet implement" must be_==("")
      }
    }
  }

  if (false) {
    "output" should {
      "出力する" in {
        val JSON = """
					{"A": 
						{
							"_kind": "sh",
							"_main": "open",
							"-a":"Safari.app file:///Applications/eclipseScala/scalaworkspace/MondogrossoProcessOrders/build/reports/tests/com.kissaki.mondogrosso.mondogrossoProcessOrders.MondogrossoProcessOrdersTests.html"
						},
					"Z": 
						{
							"_kind": "sh",
							"_main": "pwd",
							"__finallyTimeout":"2000"
						}
					}
				"""

        val result = MondogrossoProcessOrders.main(Array(
          "-p", standardInput,
          "-s", JSON))

        println("result2	" + result)
        "" must be_==("")
      }
    }
  }
}