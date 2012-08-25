package com.kissaki.mondogrosso.mondogrossoProcessOrders



trait MondogrossoCaseClasses {}

//GWTのテストの開始信号を受ける
case class EXEC_INPUT_GWTTEST(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses

//特定ファイルの監視開始
case class EXEC_START_OBSERVE(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses

//監視終了
case class EXEC_END_OBSERVE(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses

case class EXEC_START_TESTRUN(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses
case class EXEC_TESTFINISHED_TESTRUN(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses
case class EXEC_END_TESTRUN(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses

case class EXEC_START_PROCESS(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses
case class EXEC_KILL_PROCESS(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses
case class EXEC_END_PROCESS(nbr1: Int, nbr2: Int) extends MondogrossoCaseClasses