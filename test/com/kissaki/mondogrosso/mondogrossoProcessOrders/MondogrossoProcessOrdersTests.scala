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
		}"""

	val input = Array(
        "-p", standardInput,
        "-s", standardJSON)
	

  if (true) {
    "-pと-sが有れば、入力をプロセス自体として受けて、プロセスを実行する" should {
      "A!Z" in {
        val result = MondogrossoProcessOrders.main(input)

        println("result	" + result)
        "not yet implement" must be_==("")
      }
    }
  }

  if (true) {
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

  if (true) {
    "output" should {
      "出力する" in {
        val JSON =	"""
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
					"-s", standardJSON,
					"-i", "some"))

				println("result2	"+result)
				"" must be_==("")
      }
    }
  }
}