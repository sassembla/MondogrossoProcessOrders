package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {
	val standardJSON = """{"A": {"_type": "sh","_class": "AShell.sh","_exec": "myExec","key": "value","key2": "value2"}}"""

	"orderInputs" should {
		"have single triples" in {
			val input = "(else:over:vie)"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get
			result.myOrderInputTripleList.length must be_==(1)

			val s = result.myOrderInputTripleList(0)
			result.myOrderInputTripleList(0).myInputIdentity.myStr must be_==("else")
		}

		"have multiple triples" in {
			val input = "(else:over:vie,else2:over2:vie2)"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get

			result.myOrderInputTripleList.length must be_==(2)

			result.myOrderInputTripleList(1).myInputIdentity.myStr must be_==("else2")
		}

		"have multiple triples with whitespace" in {
			val input = "(else:over:vie, else2:over2:vie2)"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get

			result.myOrderInputTripleList.length must be_==(2)

			result.myOrderInputTripleList(1).myInputIdentity.myStr must be_==("else2")
		}
	}

	"waitOrders" should {
		"waits 1つのwaitをもつ" in {
			val input = "<A"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)

			val result = parser.parseAll(parser.waitOrdersOrNot, input).get

			result.myWaitOrdersList.length must be_==(1)

			result.myWaitOrdersList(0).myId must be_==("A")
		}

		"waits 2つのwaitをもつ" in {
			val input = "<A,B"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.waitOrdersOrNot, input).get

			result.myWaitOrdersList.length must be_==(2)
			result.myWaitOrdersList(0).myId must be_==("A")
			result.myWaitOrdersList(1).myId must be_==("B")
		}
	}

	"サンプル" should {
		"all	finally付きのフルセット	" in {
			val id = "finally付きのフルセット"
			val input = "A(else:over:vie,else2:over2:vie2)>B>C(a:v:s)<S,T+AB(elseB:overB:vieB,elseB2:overB2:vieB2)<SB!Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)

			val context = parser.parseProcess

			//finallyがあるはず
			context.finallyOrder must be_==("Z")

			//総合Order数
			context.totalOrderCount must be_==(4)

			//総合プロセス数
			context.totalProcessNum must be_==(2)

			//Contextに投入する初期パラメータ一式
			val contextSrc = context.initialParam

			//プロセスがあるはず
			val result = context.current

			//Ordersが2つあるはず
			result.processList.length must be_==(2)

			val the1stOrders = result.processList(0)
			val the2ndOrders = result.processList(1)

			//A,B,C,のOrderが入っているはず
			the1stOrders.orderIdentityList.length must be_==(3)

			//各identityがあっているはず		
			the1stOrders.orderIdentityList(0) must be_==("A")
			the1stOrders.orderIdentityList(1) must be_==("B")
			the1stOrders.orderIdentityList(2) must be_==("C")

			//Aには2つのinputTripleがある
			the1stOrders.orderAdditional("A").inputsList(0).sourceOrderIdentity must be_==("else")
			the1stOrders.orderAdditional("A").inputsList(0).from must be_==("over")
			the1stOrders.orderAdditional("A").inputsList(0).to must be_==("vie")

			the1stOrders.orderAdditional("A").inputsList(1).sourceOrderIdentity must be_==("else2")
			the1stOrders.orderAdditional("A").inputsList(1).from must be_==("over2")
			the1stOrders.orderAdditional("A").inputsList(1).to must be_==("vie2")

			//Cには1つのinputTripleがある
			the1stOrders.orderAdditional("C").inputsList(0).sourceOrderIdentity must be_==("a")
			the1stOrders.orderAdditional("C").inputsList(0).from must be_==("v")
			the1stOrders.orderAdditional("C").inputsList(0).to must be_==("s")

			//CにはS,T, 2つのwaitがある
			the1stOrders.orderAdditional("C").waitIdentitiesList(0) must be_==("S")
			the1stOrders.orderAdditional("C").waitIdentitiesList(1) must be_==("T")

			//2つめのOrders
			//AB,のOrderが入っているはず
			the2ndOrders.orderIdentityList.length must be_==(1)

			//プロセス２のIdentity AB
			the2ndOrders.orderIdentityList(0) must be_==("AB")

			//ABには2つのinputTripleがある
			the2ndOrders.orderAdditional("AB").inputsList(0).sourceOrderIdentity must be_==("elseB")
			the2ndOrders.orderAdditional("AB").inputsList(0).from must be_==("overB")
			the2ndOrders.orderAdditional("AB").inputsList(0).to must be_==("vieB")

			the2ndOrders.orderAdditional("AB").inputsList(1).sourceOrderIdentity must be_==("elseB2")
			the2ndOrders.orderAdditional("AB").inputsList(1).from must be_==("overB2")
			the2ndOrders.orderAdditional("AB").inputsList(1).to must be_==("vieB2")

			//ABにはひとつのwaitがある
			the2ndOrders.orderAdditional("AB").waitIdentitiesList(0) must be_==("SB")
		}

		"all	wait整合性のあるProcess連続セット	" in {
			/*
			 * 実際の流れは、
			 *1 A>B>C  E>F>G	//Cまで動いてDの完了を待つ	→	Dの完了後、E,F,Gまで動いて完了
			 *2	　　>D			//Bの完了後発生、Dが動いて完了
			 *3		  	>H>I	//								Eの完了後発生、H,I,まで動いて完了
			 *f				Z 	//									全プロセスの完了時/エラー時/タイムアウト時に実行
			 */
			val id = UUID.randomUUID().toString
			val input = "A>B>C<D>E>F>G+B>D+E>H>I!Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parseProcess

			//Ordersが3つあるはず
			result.current.processList.length must be_==(3)

			val the1stOrders = result.current.processList(0)
			val the2ndOrders = result.current.processList(1)
			val the3rdOrders = result.current.processList(2)

			//A,B,C, E,F,G のOrderが入っているはず
			the1stOrders.orderIdentityList.length must be_==(6)

			//Cがwait Dを持つはず
			the1stOrders.orderAdditional("C").waitIdentitiesList(0) must be_==("D")

			//B(processWait),D,のOrderが入っているはず
			the2ndOrders.orderIdentityList.length must be_==(2)

			//E,H,I,のOrderが入っているはず
			the3rdOrders.orderIdentityList.length must be_==(3)
		}

	}

	"parseError" in {
		"invalidな構文サンプル" in {
			val id = UUID.randomUUID().toString
			val input = ">>>>ASDG+-LASdB?Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)

			try {
				val result = parser.parseProcess
				false must be_==(true)
			} catch {
				case e => {
					println("excet?	" + e)
					e must be_==("java.lang.RuntimeException : Invalid Ideitifier")
				}

			} finally {
				println("finally	done")
			}
		}

		"""all	wait整合性の "無い" Process連続セットのパースでwait整合性エラー""" in {
			/*
			 * 実際の流れは、
			 *1 A>B>C  E>F>G	//Cまで動いてXの完了を待つ	→	Xの完了後、E,F,Gまで動いて完了 ===== Xなんて無いのでロックする
			 *2	　　>D			//Bの完了後発生、Dが動いて完了
			 *3		  	>H>I	//								Eの完了後発生、H,I,まで動いて完了
			 *f				Z 	//									全プロセスの完了時/エラー時/タイムアウト時に実行
			 */
			val id = UUID.randomUUID().toString
			val input = "A>B>C<X>E>F>G+B>D+E>H>I!Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)
			try {
				val result = parser.parseProcess
				"never reach here." must be_==("")
			} catch {
				case e => {
					println("excet?	" + e)
					e must be_==("java.lang.RuntimeException: Illegular wait-relation when parsing failed")
				}
			} finally {
				println("finally	done")
			}
		}

		"finallyが無い" in {
			val id = UUID.randomUUID().toString
			val input = "A>B>C"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(id, input, json)

			try {
				val result = parser.parseProcess
				"never reach here." must be_==("")
			} catch {
				case e => {
					println("except?	" + e)
					e must be_==("""java.lang.RuntimeException : Missing "Finally" OrderIdentity""")
				}

			} finally {
				println("finally	done")
			}
		}

	}

	"samples" should {
		"processes	複数のProcessフルセット	、finallyなし" in {
			val input = "A(else:over:vie,else:over:vie)>B<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.processes, input)
			"not yet tested well" must be_==("")
		}

		"all	複数のWaitあり、なしのProcessフルセット	" in {
			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"all	複数のWaitなしのProcessフルセット	" in {
			val input = "A(else:over:vie,else:over:vie)+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"all	複数のWait、パラメータなしのProcessフルセット	" in {
			val input = "A>B+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"orders	<のあとにIdentityが一つあるケース" in {

			val input = "A(else:over:vie,else:over:vie)<S"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)
			"not yet tested well" must be_==("")
		}

		"orders	<のあとにIdentityが一つあるケース2 パラメータなし" in {

			val input = "A<S"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)
			"not yet tested well" must be_==("")
		}

		"orders	<のあとにIdentityが一つあるケース3 パラメータなし、複数のOrder" in {

			val input = "A>B<S"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)
			"not yet tested well" must be_==("")
		}

		"order	<が無いケース" in {

			val input = "A(else:over:vie,else:over:vie)"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.order, input)
			"not yet tested well" must be_==("")
		}

		"all	<が無いケース" in {

			val input = "A(else:over:vie,else:over:vie)!F"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"all	改行入りのケース1 改行コード" in {
			val input = "A(else:over:vie else:over:vie)<S\n+AB(elseB:overB:vieB elseB:overB:vieB)<SB\n+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"all	改行入りのケース2 改行される文章" in {
			val input =
				"""A(else:over:vie,else:over:vie)<S
+AB(elseB:overB:vieB elseB:overB:vieB)<SB
+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"""

			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}

		"単純なOrderの連続" in {
			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
			val json = standardJSON
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess
			"not yet tested well" must be_==("")
		}
	}

	"JSON" should {
		"単一のパラメータ" in {
			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
			val json = standardJSON

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

			result.initialParam must be_==(Map("A" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2")))
		}

		"複数のパラメータ" in {
			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
			val json = """{
    "A": {
        "_type": "sh",
        "_class": "AShell.sh",
        "_exec": "myExec",
        "key": "value",
        "key2": "value2"
    },
    "B": {
        "_type": "sh",
        "_class": "AShell.sh",
        "_exec": "myExec",
        "key": "value",
        "key2": "value2"
    },
    "C": {
        "_type": "sh",
        "_class": "AShell.sh",
        "_exec": "myExec",
        "key": "value",
        "key2": "value2"
    },
    "D": {
        "_type": "sh",
        "_class": "AShell.sh",
        "_exec": "myExec",
        "key": "value",
        "key2": "value2"
    },
    "E": {
        "_type": "sh",
        "_class": "AShell.sh",
        "_exec": "myExec",
        "key": "value",
        "key2": "value2"
    }
}"""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

			val a = Map("A" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2"))
			val b = Map("B" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2"))
			val c = Map("C" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2"))
			val d = Map("D" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2"))
			val e = Map("E" -> Map("_type" -> "sh", "_class" -> "AShell.sh", "_exec" -> "myExec", "key" -> "value", "key2" -> "value2"))

			val total = a ++ b ++ c ++ d ++ e
			result.initialParam must be_==(total)
		}
	}

}