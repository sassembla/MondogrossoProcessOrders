package com.kissaki

/**
 * Akka系の実装ではない、ちょっと残念なMessenger
 * 0.5.3	12/09/26 22:48:00	asyncでの指定時間遅延が実行可能
 * 				非同期系を使う際、外界をロックせずに指定時間後のコード実行予約が可能。
 * 
 * 0.5.2	12/09/09 20:43:33	Scala版のサンプルとbuild.gradleを追加
 * 				gradlew jar でJarのビルドができます。
 * 				ビルド済みのJarは、	build/libs/の中にあるはず。
 *
 * 0.5.1	12/09/09 0:54:49	ネストの問題を解決。myselfは無限ネストが可能。
 *
 * 0.5.0	12/09/08 23:04:15	非同期までの実装完了。
 * 				同期呼び出しはネストすると2ネスト以降でロックする(すでにFutureがセットされているため)
 * 				A1-B1-A2で、A2以降で同期を使うと問題が出る。
 *
 *
 * @author sassembla
 */
 

import scala.actors.Actor._
import scala.actors._
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

/*
 * システム系のcase class
 */
case class MessengerJoin(actor : MessengerActor)
case class MessengerRemove(actor : MessengerActor)

/*
 * system series
 */
case class InputParent(parentCandidateName : String, child : MessengerActor)
case class ChildInput(child : MessengerActor)
case class ParentAccepted(parent : MessengerActor)
case class ChildAccepted(child : MessengerActor)
case class ChildNotAccepted(child : MessengerActor)
case class ParentChildDone()
case class ParentChildNotDone()
case class RemoveFromParent(myself : MessengerActor)
case class RemoveFromChild(myself : MessengerActor)

/*
 * call series
 */
case class Call(exec : String, message : Array[TagValue])
case class CallMyself(exec : String, message : Array[TagValue])
case class CallParent(exec : String, message : Array[TagValue])

case class CallWithAsync(exec : String, message : Array[TagValue])
case class CallMyselfWithAsync(exec : String, message : Array[TagValue])
case class CallParentWithAsync(exec : String, message : Array[TagValue])

/*
 * result
 */
case class Done(detail : String)
case class Failure(reason : String)

/*
 * result
 */
case class Result(result : String)

/**
 * メッセンジャー実装
 *
 * Actorのコントロール/receiverの着火を行う
 */
class Messenger(myself : MessengerProtocol, nameInput : String) {

	val self : MessengerProtocol = myself

	/*
	 * シングルトンの生成、centralActorの参照の取得
	 * actorの埋め込み(ActorのLinkみたいなのが出来るといいんだけどね。受け取り側で判断させられる)
	 */
	val centralActorImpl = MessengerCore.centralActor

	val actorImpl = new MessengerActor(self, nameInput)
	actorImpl.start
	actorImpl.log += Log.LOG_TYPE_INITIALIZED.toString
	/*
	 * シングルトンへのActorの追加
	 * actorの埋め込み(ActorのLinkみたいなのが出来るといいんだけどね。)
	 */
	val future = centralActorImpl !! MessengerJoin(actorImpl)
	val result = future()

	/**
	 * このMessengerを閉じる
	 */
	def close {
		if (hasParent) {
			actorImpl.removeFromParent
		}

		if (hasChild) {
			actorImpl.removeFromChild
		}

		val future1 = centralActorImpl !! MessengerRemove(actorImpl)
		val result1 = future1()

		val future2 = actorImpl !! World.WORLD_MESSAGE_MEMBER_EXIT
		val result2 = future2()
	}

	/**
	 * 系全体のクローズを行う
	 */
	def closeSystem {
		MessengerCore.closeSystem
	}

	def getName : String = actorImpl.name
	def getId : String = actorImpl.id

	def getParentName : String = actorImpl.parentName
	def getParentId : String = actorImpl.parentId

	def hasChild : Boolean = !actorImpl.childList.isEmpty
	def hasParent : Boolean = !actorImpl.parent.isEmpty

	def getChildNum = actorImpl.childList.size

	/**
	 * 親の名前入力
	 */
	def inputParent(inputParentName : String) : Result = {
		if (actorImpl.parent.isEmpty) {
			/*
			 * 自前のActorへとインプット
			 */
			val future = centralActorImpl !! InputParent(inputParentName, actorImpl)
			val result = future()

			result match {
				case Done(message) => Result(message)
				case Failure(reason) => Result(reason)
			}
		} else {
			Result(actorImpl.name + " aleady has parent that named " + actorImpl.parent(0).name)
		}

	}

	/**
	 * log
	 */
	def addLog(input : String) = { actorImpl.log += input }
	def getLog : ListBuffer[String] = actorImpl.log
	def getLogAsJava : java.util.List[String] = {
		actorImpl.log.asJava
	}

	def getCentralActorSize : Int = {
		centralActorImpl.actorList.length
	}

	//同期版
	/**
	 * 特定の子へとメッセージを飛ばす
	 */
	def call(targetName : String, exec : String, message : Array[TagValue]) = {
		actorImpl.call(targetName, exec, message)
	}

	/**
	 * 親へとメッセージを飛ばす
	 */
	def callParent(exec : String, message : Array[TagValue]) = {
		actorImpl.callParent(exec, message)
	}

	/**
	 * 自分自身へとメッセージを飛ばす
	 */
	def callMyself(exec : String, message : Array[TagValue]) {
		actorImpl.callMyself(exec, message)
	}

	//非同期版
	/**
	 * 特定の子へと非同期にメッセージを飛ばす
	 */
	def callWithAsync(targetName : String, exec : String, message : Array[TagValue]) = {
		actorImpl.callWithAsync(targetName, exec, message)
	}

	/**
	 * 親へと非同期にメッセージを飛ばす
	 */
	def callParentWithAsync(exec : String, message : Array[TagValue]) = {
		actorImpl.callParentWithAsync(exec, message)
	}

	/**
	 * 自分自身へと非同期にメッセージを飛ばす
	 */
	def callMyselfWithAsync(exec : String, message : Array[TagValue]) {
		actorImpl.callMyselfWithAsync(exec, message)
	}

	/**
	 * tagの一覧を返す
	 */
	def tags(tagValues : Array[TagValue]) = for (tagValue <- tagValues) yield tagValue.getTag

	/**
	 * tag-valueのペア
	 * Array[TagValue]を返す
	 * (Seqを返すほうが処理少なくていいんだけど。)
	 */
	def tagValues(tagValue : TagValue*) = {
		tagValue.toArray
	}

	/**
	 * 値を返す
	 */
	def get(tag : String, tagvalues : Array[TagValue]) = {
		val ret = for (tagValue <- tagvalues.withFilter(_.m_tag.equals(tag))) yield tagValue.get(tag)
		
			
		if (ret.isEmpty) {
			val out = (for (tagValue<- tagvalues) yield tagValue.toString).reduceLeft{
				(total, toAdd) => 
					total.toString +", "+ toAdd.toString
			}
			
			sys.error("this is :"+ nameInput + "	/parent: "+ getParentName + "	/no-tag	:"+tag + "	/inside of	:"+out + "	/size	"+tagvalues.size)
			sys.exit(-1)
		}
		ret(0)
	}
}

/**
 * 系
 * シングルトン
 */
object MessengerCore {
	val identity = UUID.randomUUID().toString()
	val centralActor = new MessengerCentral
	centralActor.start

	/**
	 * 系を開始する
	 */
	def initSystem = {}

	/**
	 * 系を閉じる
	 */
	def closeSystem {
		/*
		 * actor中継を不可能にする
		 * リスト内のactorを破壊
		 * リストを空に
		 * MessengerCore自体のidを破棄(したいけどシングルトンなのでどうしたものか、、外部から破壊したいのだが。
		 */
		centralActor.actorList.foreach { actor =>
			val result = actor !! World.WORLD_MESSAGE_MEMBER_EXIT
		}
		centralActor.actorList.clear()
		println("everything removed" + centralActor.actorList.size)
	}
}

/**
 * 中央Actor
 *
 * 全参加Actorはここに追加される
 * actor間の中継を行う
 */
class MessengerCentral extends Actor {

	val actorList : ListBuffer[MessengerActor] = ListBuffer()

	def act() = {
		loop {
			react {

				case InputParent(targetName, childActor) => {
					childActor.log += Log.LOG_TYPE_INPUT_TO_PARENTCANDIDATE.toString

					//登録済みのactorの中から、対象を限定してブロードキャストを行う
					actorList.withFilter(_.name.equals(targetName)).foreach { targetCandidate =>
						val future = targetCandidate !! ChildInput(childActor)

						val result = future()

						//この時点でとある親と子のやり取りは完了している

						result match {
							case ParentChildDone() => {
								reply(Done("inputParent succeeded"))
							}
							case ParentChildNotDone() => //同名の親の登録に先を超されている場合
						}
					}

					reply(Failure("targetted parent named:" + targetName + " is not exist. please check parent's name"))
				}

				//system
				case MessengerJoin(actor) => {
					actorList += actor
					reply(Done("joined rep	" + actorList.length))
				}

				case MessengerRemove(actor) => {
					actorList -= actor
					
					reply(Done("removed rep	" + actorList.length))
				}

				case m => {
					println("不可解なメッセージ")
					reply(Failure("something wrong,, " + m))
				}
			}
		}
	}
}

/**
 * Messengerの役割を持ったアクター
 * Centralを経由してお互いのポインタを渡し合い、メッセージングを行う。
 */
class MessengerActor(myself : MessengerProtocol, inputtedName : String) extends Actor {
	val log : ListBuffer[String] = ListBuffer()

	val childList : ListBuffer[MessengerActor] = ListBuffer()

	val name = inputtedName
	val id = UUID.randomUUID().toString()

	val parent : ListBuffer[MessengerActor] = ListBuffer()

	def parentName : String = parent(0).name
	def parentId : String = parent(0).id

	def duplicate : SubMessengerActor = {
		new SubMessengerActor(this, myself)
	}

	/**
	 * 親からの離脱
	 */
	def removeFromParent = {
		val future = parent(0) !! RemoveFromParent(this)
		val result = future()
	}

	def removeFromChild = {
		childList.foreach { targetChild =>
			val future = targetChild.duplicate !! RemoveFromChild(this)
			val result = future()

			result match {
				case Done(_) =>
				case Failure(reason) =>
			}
		}
	}

	/**
	 * 同期系
	 */
	def call(targetName : String, exec : String, message : Array[TagValue]) = {
		childList.withFilter(_.name.equals(targetName)).foreach { targetChild =>
			log += Log.LOG_TYPE_CALLCHILD.toString

			val future = targetChild.duplicate !! Call(exec, message)
			val result = future()

			result match {
				case Done(_) =>
				case Failure(reason) =>
			}
		}
	}

	def callMyself(exec : String, message : Array[TagValue]) = {
		log += Log.LOG_TYPE_CALLMYSELF.toString

		val future = this.duplicate !! CallMyself(exec, message)
		val result = future()

		result match {
			case Done(_) =>
			case Failure(reason) =>
		}
	}

	def callParent(exec : String, message : Array[TagValue]) = {		
		log += Log.LOG_TYPE_CALLPARENT.toString
		val future = parent(0).duplicate !! CallParent(exec, message)
		val result = future()

		result match {
			case Done(_) =>
			case Failure(reason) =>
		}
	}

	/**
	 * 非同期系
	 */
	def callWithAsync(targetName : String, exec : String, message : Array[TagValue]) = {
		childList.withFilter(_.name.equals(targetName)).foreach { targetChild =>
			log += Log.LOG_TYPE_CALLCHILD_ASYNC.toString
			targetChild.duplicate !! CallWithAsync(exec, message)
		}
	}

	def callMyselfWithAsync(exec : String, message : Array[TagValue]) = {
		log += Log.LOG_TYPE_CALLMYSELF_ASYNC.toString
		this.duplicate !! CallMyselfWithAsync(exec, message)
	}

	def callParentWithAsync(exec : String, message : Array[TagValue]) = {
		log += Log.LOG_TYPE_CALLPARENT_ASYNC.toString
		parent(0).duplicate !! CallParentWithAsync(exec, message)
	}

	//receive
	def act() = {
		loop {
			react {
				/*
				 * centralを通じた、子ども候補からのインプット
				 * 受け取りの時点で自分の名前をしたMessengerがターゲットなのは確定しているが、
				 * 子ども候補が既に別の同名の親候補から返事を受け取っている可能性もある。
				 */
				case ChildInput(childActor) => {
					//子ども候補からの通信
					log += Log.LOG_TYPE_PARENTCANDIDATE_RECEIVED.toString + childActor
					val future = childActor !! ParentAccepted(this)
					log += Log.LOG_TYPE_PARENTCANDIDATE_ANSWERED.toString + childActor
					val result = future()

					result match {
						case ChildAccepted(currentChildActor) => {
							log += Log.LOG_TYPE_PARENT_CHILD_CONNECTED.toString + currentChildActor

							childList += currentChildActor
							reply(ParentChildDone())
						}
						case ChildNotAccepted(_) => {
							reply(ParentChildNotDone())
						}
					}
				}

				case ParentAccepted(parentActor) => {
					log += Log.LOG_TYPE_CHILD_RECEIVED.toString + parentActor
					if (parent.isEmpty) { //まだ誰も親がいない
						log += Log.LOG_TYPE_PARENT_CHILD_CONNECTED.toString + parentActor
						parent += parentActor
						reply(ChildAccepted(this))
					} else {
						reply(ChildNotAccepted(this))
					}
				}

				case RemoveFromParent(id) => {
					log += Log.LOG_TYPE_REMOVE_CHILD.toString
					childList -= id
					reply(Done("child-removed"))
				}

				/*
				 * messengerとしての駆動を終える
				 */
				case World.WORLD_MESSAGE_MEMBER_EXIT => {
					reply(Done("このMessenger停止完了"))
					exit
				}

				case something : String => {
					println("不明物が届いた" + something)
				}
			}
		}
	}

	/**
	 * 送信処理の代理人
	 */
	class SubMessengerActor(master : MessengerActor, myself : MessengerProtocol) extends Actor {
		start

		def act() = {
			loop {
				react {
					//call / async
					case Call(exec, message) => {
						println("called")
						master.log += Log.LOG_TYPE_CALLED_AS_CHILD.toString

						myself.receiver(exec, message)

						reply(Done("child called"))
						exit
					}
					case CallWithAsync(exec, message) => {
						println("CallWithAsync")
						master.log += Log.LOG_TYPE_CALLED_AS_CHILD_ASYNC.toString
	
						myself.receiver(exec, message)
	
						reply(Done("child-sync called"))
						exit
					}

					//callMyself / async
					case CallMyself(exec, message) => {
						println("CallMyself")
						master.log += Log.LOG_TYPE_CALLED_MYSELF.toString

						myself.receiver(exec, message)

						reply(Done("myself called"))
						exit
					}
					case CallMyselfWithAsync(exec, message) => {
						println("CallMyselfWithAsync")
						master.log += Log.LOG_TYPE_CALLED_MYSELF_ASYNC.toString

						myself.receiver(exec, message)

						reply(Done("myself-async called"))
						exit
					}

					//callParent / async
					case CallParent(exec, message) => {
						println("CallParent")
						master.log += Log.LOG_TYPE_CALLED_AS_PARENT.toString

						myself.receiver(exec, message)

						reply(Done("parent called"))
					}
					case CallParentWithAsync(exec, message) => {
						println("CallParentWithAsync")
						master.log += Log.LOG_TYPE_CALLED_AS_PARENT_ASYNC.toString
	
						myself.receiver(exec, message)
	
						reply(Done("parent-sync called"))
						exit
					}

					case RemoveFromChild(id) => {
						println("RemoveFromChild")
						master.log += Log.LOG_TYPE_REMOVE_PARENT.toString
						parent -= id
						reply(Done("parent-removed"))
					}

					case message => {
						println("hereComes	" + message)
					}
				}
			}
		}
	}
}