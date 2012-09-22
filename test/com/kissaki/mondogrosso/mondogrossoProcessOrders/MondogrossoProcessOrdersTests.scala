package com.kissaki.mondogrosso.mondogrossoProcessOrders

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import scala.collection.immutable.ListMap
import scala.sys.process._
import java.util.UUID

@RunWith(classOf[JUnitRunner])
class MondogrossoProcessOrdersTests extends Specification {

	"orderInputs" should {
		"have single triples" in {
			val input = "(else:over:vie)"
			val json = "{\"A\":{\"type\":\"sh\",\"class\":\"AShell.sh\",\"exec\":\"myExec\"}}"

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get
			result.myOrderInputTripleList.length == 1 must beTrue

			val s = result.myOrderInputTripleList(0)
			result.myOrderInputTripleList(0).myInputIdentity.myStr == "else" must beTrue
		}

		"have multiple triples" in {
			val input = "(else:over:vie,else2:over2:vie2)"

			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get

			result.myOrderInputTripleList.length == 2 must beTrue

			result.myOrderInputTripleList(1).myInputIdentity.myStr == "else2" must beTrue
		}

		"have multiple triples with whitespace" in {
			val input = "(else:over:vie, else2:over2:vie2)"

			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orderInputs, input).get

			result.myOrderInputTripleList.length == 2 must beTrue

			result.myOrderInputTripleList(1).myInputIdentity.myStr == "else2" must beTrue
		}
	}
	
	"waitOrders" should {
		"waits 1つのwaitをもつ" in {
			val input = "<A"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			
			val result = parser.parseAll(parser.waitOrdersOrNot, input).get
			
			result.myWaitOrdersList.length == 1 must beTrue
			
			result.myWaitOrdersList(0).myId == "A" must beTrue
		}
		
		"waits 2つのwaitをもつ" in {
			val input = "<A,B"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.waitOrdersOrNot, input).get
			
			result.myWaitOrdersList.length == 2 must beTrue
			result.myWaitOrdersList(0).myId == "A" must beTrue
			result.myWaitOrdersList(1).myId == "B" must beTrue
		}
	}

	"サンプル" should {
		"all	finally付きのフルセット	" in {
			val id = "finally付きのフルセット"
			val input = "A(else:over:vie,else2:over2:vie2)>B>C(a:v:s)<S,T+AB(elseB:overB:vieB,elseB2:overB2:vieB2)<SB!Z"

			val json = ""
			val parser = new MondogrossoProcessParser(id, input, json)

			val context = parser.parseProcess

			//finallyがあるはず
			context.finallyOrder == "Z" must beTrue

			//総合Order数
			context.totalOrderCount == 4 must beTrue

			//総合プロセス数
			context.totalProcessNum == 2 must beTrue

			//Contextに投入する初期パラメータ一式
			val contextSrc = context.initialParam

			//プロセスがあるはず
			val result = context.current
			
			//Ordersが2つあるはず
			result.processList.length == 2 must beTrue

			val the1stOrders = result.processList(0)
			val the2ndOrders = result.processList(1)

			//A,B,C,のOrderが入っているはず
			the1stOrders.orderIdentityList.length == 3 must beTrue

			//各identityがあっているはず		
			the1stOrders.orderIdentityList(0) == "A" must beTrue
			the1stOrders.orderIdentityList(1) == "B" must beTrue
			the1stOrders.orderIdentityList(2) == "C" must beTrue

			//Aには2つのinputTripleがある
			the1stOrders.orderAdditional("A").inputsList(0).sourceOrderIdentity == "else" must beTrue
			the1stOrders.orderAdditional("A").inputsList(0).from == "over" must beTrue
			the1stOrders.orderAdditional("A").inputsList(0).to == "vie" must beTrue

			the1stOrders.orderAdditional("A").inputsList(1).sourceOrderIdentity == "else2" must beTrue
			the1stOrders.orderAdditional("A").inputsList(1).from == "over2" must beTrue
			the1stOrders.orderAdditional("A").inputsList(1).to == "vie2" must beTrue

			//Cには1つのinputTripleがある
			the1stOrders.orderAdditional("C").inputsList(0).sourceOrderIdentity == "a" must beTrue
			the1stOrders.orderAdditional("C").inputsList(0).from == "v" must beTrue
			the1stOrders.orderAdditional("C").inputsList(0).to == "s" must beTrue

			//CにはS,T, 2つのwaitがある
			the1stOrders.orderAdditional("C").waitIdentitiesList(0) == "S" must beTrue
			the1stOrders.orderAdditional("C").waitIdentitiesList(1) == "T" must beTrue
			
			//2つめのOrders
			//AB,のOrderが入っているはず
			the2ndOrders.orderIdentityList.length == 1 must beTrue

			//プロセス２のIdentity AB
			the2ndOrders.orderIdentityList(0) == "AB" must beTrue

			//ABには2つのinputTripleがある
			the2ndOrders.orderAdditional("AB").inputsList(0).sourceOrderIdentity == "elseB" must beTrue
			the2ndOrders.orderAdditional("AB").inputsList(0).from == "overB" must beTrue
			the2ndOrders.orderAdditional("AB").inputsList(0).to == "vieB" must beTrue

			the2ndOrders.orderAdditional("AB").inputsList(1).sourceOrderIdentity == "elseB2" must beTrue
			the2ndOrders.orderAdditional("AB").inputsList(1).from == "overB2" must beTrue
			the2ndOrders.orderAdditional("AB").inputsList(1).to == "vieB2" must beTrue

			//ABにはひとつのwaitがある
			the2ndOrders.orderAdditional("AB").waitIdentitiesList(0) == "SB" must beTrue
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
			val json = ""

			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parseProcess

			//Ordersが3つあるはず
			result.current.processList.length == 3 must beTrue

			val the1stOrders = result.current.processList(0)
			val the2ndOrders = result.current.processList(1)
			val the3rdOrders = result.current.processList(2)

			//A,B,C, E,F,G のOrderが入っているはず
			the1stOrders.orderIdentityList.length == 6 must beTrue

			//Cがwait Dを持つはず
			the1stOrders.orderAdditional("C").waitIdentitiesList(0) == "D" must beTrue

			//B(processWait),D,のOrderが入っているはず
			the2ndOrders.orderIdentityList.length == 2 must beTrue

			//E,H,I,のOrderが入っているはず
			the3rdOrders.orderIdentityList.length == 3 must beTrue
		}

		

	}

	"parseError" in {
		"invalidな構文サンプル" in {
			val id = UUID.randomUUID().toString
			val input = ">>>>ASDG+-LASdB?Z"
			val json = ""

			val parser = new MondogrossoProcessParser(id, input, json)

			try {
				val result = parser.parseProcess
				false == true must beTrue
			} catch {
				case e => {
					println("excet?	"+e)
					e == "java.lang.RuntimeException : Invalid Ideitifier" must beTrue
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
			val json = ""

			val parser = new MondogrossoProcessParser(id, input, json)
			try {
				val result = parser.parseProcess
				println("ここに到達しちゃいけない	")
				false == true must beTrue
			} catch {
				case e => {
					println("excet?	"+e)
					e == "java.lang.RuntimeException: Illegular wait-relation when parsing failed" must beTrue
				}
			} finally {
				println("finally	done")
			}
		}
		
		"finallyが無い" in {
			val id = UUID.randomUUID().toString
			val input = "A>B>C"
			val json = ""

			val parser = new MondogrossoProcessParser(id, input, json)

			try {
				val result = parser.parseProcess
				false == true must beTrue
			} catch {
				case e => {
					println("excet?	"+e)
					e == """java.lang.RuntimeException : Missing "Finally" OrderIdentity""" must beTrue
				}

			} finally {
				println("finally	done")
			}
		}
		
	}

	"samples" should {
		"processes	複数のProcessフルセット	、finallyなし" in {
			val input = "A(else:over:vie,else:over:vie)>B<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.processes, input)

		}

		"all	複数のWaitあり、なしのProcessフルセット	" in {
			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"all	複数のWaitなしのProcessフルセット	" in {
			val input = "A(else:over:vie,else:over:vie)+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"all	複数のWait、パラメータなしのProcessフルセット	" in {
			val input = "A>B+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"orders	<のあとにIdentityが一つあるケース" in {

			val input = "A(else:over:vie,else:over:vie)<S"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)

		}

		"orders	<のあとにIdentityが一つあるケース2 パラメータなし" in {

			val input = "A<S"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)

		}

		"orders	<のあとにIdentityが一つあるケース3 パラメータなし、複数のOrder" in {

			val input = "A>B<S"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.orders, input)

		}

		"order	<が無いケース" in {

			val input = "A(else:over:vie,else:over:vie)"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseAll(parser.order, input)

		}

		"all	<が無いケース" in {

			val input = "A(else:over:vie,else:over:vie)!F"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"all	改行入りのケース1 改行コード" in {
			val input = "A(else:over:vie else:over:vie)<S\n+AB(elseB:overB:vieB elseB:overB:vieB)<SB\n+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"all	改行入りのケース2 改行される文章" in {
			val input =
				"""A(else:over:vie,else:over:vie)<S
+AB(elseB:overB:vieB elseB:overB:vieB)<SB
+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"""

			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}

		"単純なOrderの連続" in {
			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parseProcess

		}
	}

}