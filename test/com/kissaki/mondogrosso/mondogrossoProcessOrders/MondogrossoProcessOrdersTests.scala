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

			val json = ""

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
	
	"サンプル" should {
		"all	finally付きのフルセット	" in {
			val id = "finally付きのフルセット"
			val input = "A(else:over:vie,else2:over2:vie2)>B>C(a:v:s)<S+AB(elseB:overB:vieB,elseB2:overB2:vieB2)<SB!Z"
			
			val json = ""
			val parser = new MondogrossoProcessParser(id, input, json)
			val result2 = parser.parse
			
			//コンテキストのidがあるはず
			result2.contextId == id must beTrue
			
			//finallyがあるはず
			result2.finallyOrder == "Z" must beTrue
			
			//総合Order数
			result2.totalOrderCount == 4 must beTrue

			//総合プロセス数
			result2.totalProcessNum == 2 must beTrue
			
			//各プロセスのidがあるはず
			
			
			//Contextに投入する初期パラメータ一式
			val contextSrc = result2.initialParam
			println("initialParam	"+result2.initialParam)
			
			//プロセスがあるはず
			val result = result2.process
			
			println("here5	"+result)
			/*
			 * どうなるのがうれしいのか
			 * ・Contextの内容が一目でわかる
			 * 		OrderごとにどんなId,パラメータを持っているか
			 * 
			 * 		printlnしたときに、
			 * 		Order(id1,状態)
			 * 		Order(id2,状態)
			 * 		Order(id3,状態)
			 * 		みたいに見れるといいな。
			 * 		→contextにorderのidentity + パラメータ値一覧がある
			 * 			contextのidentityはもっと上層がなんとかする。
			 * 			//パラメータ側
			 * 			context:{
			 * 				initialParam : {//実行時の値の初期値。imputがある場合はcurrentから値を引っ張ってきて上書きされる。finallyの引数でもある。
			 * 					A : {
			 * 						key:value
			 * 					} x n
			 * 				}
			 * 				
			 * 				eventualParam : {//実行後の値が入る。finallyの引数でもある。 受け取り側で作れば良い。
			 * 					//処理完了した際の値が入る。ここにidentityが無ければ終わってない。
			 * 				}
			 * 
			 * 				//Current(process:List[Process])
			 * 				//	Process(identity:String, currentIndex:Int, orderIdentityList:List[String], orderAdditional:Map[String, OrderAddition])
			 * 				//		OrderAddition(inputsList:List[Input], waitIdentitiesList:ListBuffer[String])
			 * 				//			InputRule(sourceOrderIdentity:String, from:String, to:String)
			 * 
			 * 				current : {
			 * 					process : {
			 * 						identity(UUID)
			 * 						currentIndex		//finallyの引数
			 * 						orderIdentityList : {
			 * 							//実行順に並べたもの。Seq
			 * 							A,B,C,D
			 * 						}
			 * 						orderAdditional : {
			 * 							A:{
			 * 								inputs
			 * 								waits
			 * 							} x n
			 * 						}
			 * 					} x n
			 * 				}
			 * 				
			 * 				totalOrderCount
			 * 				restOrderCount
			 * 
			 * 				totalProcessNum
			 * 				restProcessNum
			 * 			}
			 * 			
			 * 					
			 * 
			 * ・Workerに放り込みやすい
			 * 		Orderそのままを放り込めればベスト。ぶん投げ。
			 * 		messenger.call(processId, START, Order)
			 * 				
			 * ・Worker側でやる処理は、
			 * 	・種類ごとに異なる処理なので、typeは必須
			 * 	・処理内容→jsonから取って来れてるはずなので、そのままそれがキーになってるはず。case class一発。
			 * 
			 * ・WorkerからDoneが来た時、次の処理にうつりやすい
			 * 		index++
			 * 		→これで読めるようにするには、
			 * 			process.orderIdentityList(index)で、次実行するIdentity取れる
			 * 			process.orderAdditional(Identity).inputsList(len)で、ルール一式が取れる
			 * 			process.orderAdditional(Identity).waitIdentitiesListで、waitIdentity一式が取れる
			 * 
			 */
			
//			//Ordersが2つあるはず
//			result.length == 2 must beTrue
//
//			//A,B,C,のOrderが入っているはず
//			result(0).length == 3 must beTrue
//			
//			//AB,のOrderが入っているはず
//			result(1).length == 1 must beTrue
//			
//			//各identityがあっているはず		
//			result(processKey1)(0)("identity") == "A" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("sourceOrderIdentity") == "else" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("from") == "over" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("to") == "vie" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("sourceOrderIdentity") == "else2" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("from") == "over2" must beTrue
//			result(processKey1)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("to") == "vie2" must beTrue
//			
//			result(processKey1)(1)("identity") == "B" must beTrue
//			
//			result(processKey1)(2)("identity") == "C" must beTrue
//			result(processKey1)(2)("inputsList").asInstanceOf[List[Map[String,String]]](0)("sourceOrderIdentity") == "a" must beTrue
//			result(processKey1)(2)("inputsList").asInstanceOf[List[Map[String,String]]](0)("from") == "v" must beTrue
//			result(processKey1)(2)("inputsList").asInstanceOf[List[Map[String,String]]](0)("to") == "s" must beTrue
//			
//			result(processKey1)(2)("waitOrdersList").asInstanceOf[List[Map[String,String]]](0)("waitIdentity") == "S" must beTrue
//			
//			result(processKey2)(0)("identity") == "AB" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("sourceOrderIdentity") == "elseB" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("from") == "overB" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](0)("to") == "vieB" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("sourceOrderIdentity") == "elseB2" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("from") == "overB2" must beTrue
//			result(processKey2)(0)("inputsList").asInstanceOf[List[Map[String,String]]](1)("to") == "vieB2" must beTrue
//			result(processKey2)(0)("waitOrdersList").asInstanceOf[List[Map[String,String]]](0)("waitIdentity") == "SB" must beTrue
		}
		
		"all	複数のProcessフルセット	" in {
			val id = UUID.randomUUID().toString
			val input = "A(else:over:vie,else:over:vie)<S+AB(elseB:overB:vieB,elseB:overB:vieB)<SB!F"
			val json = ""

			val parser = new MondogrossoProcessParser(id, input, json)
			val result = parser.parse

			//Ordersが2つあるはず
//			result.length == 2 must beTrue
//
//			//A,B,C,のOrderが入っているはず
//			result(0).length == 3 must beTrue
//			
//			//AB,のOrderが入っているはず
//			result(1).length == 1 must beTrue
//			
//			//各identityがあっているはず		
//			result(0)(0)("identity") == "A" must beTrue
//			
//			
//			result(0)(1)("identity") == "B" must beTrue
//			result(0)(2)("identity") == "C" must beTrue
//			
//			result(1)(0)("identity") == "AB" must beTrue
			
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
			val result = parser.parse

		}

		"all	複数のWaitなしのProcessフルセット	" in {
			val input = "A(else:over:vie,else:over:vie)+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parse

		}

		"all	複数のWait、パラメータなしのProcessフルセット	" in {
			val input = "A>B+AB(elseB:overB:vieB,elseB:overB:vieB)!F"
			val json = ""

			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parse

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
			val result = parser.parse
			
		}

		"all	改行入りのケース1 改行コード" in {
			val input = "A(else:over:vie else:over:vie)<S\n+AB(elseB:overB:vieB elseB:overB:vieB)<SB\n+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parse
			
		}

		"all	改行入りのケース2 改行される文章" in {
			val input =
				"""A(else:over:vie,else:over:vie)<S
+AB(elseB:overB:vieB elseB:overB:vieB)<SB
+AB2(elseB2:overB2:vieB2 elseB2:overB2:vieB2)<SB2!Z"""

			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parse
			
			
		}
		
		"単純なOrderの連続" in {
			val input = "A>B>C>D>E>F>G>H>I>J>K!Z"
			val json = ""
			val parser = new MondogrossoProcessParser(UUID.randomUUID().toString, input, json)
			val result = parser.parse
			
		}
	}
	
}