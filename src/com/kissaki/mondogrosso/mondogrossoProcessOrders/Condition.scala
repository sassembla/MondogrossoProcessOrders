package com.kissaki.mondogrosso.mondogrossoProcessOrders

object Condition extends Enumeration {
	//condition
	type Condition = Value
	val CONDITION_PREPARED,
	CONDITION_RUNNING,
	CONDITION_STOPPED_BY_ERROR,
	CONDITION_STOPPED_BY_PENDING,
	CONDITION_DONE
	= Value
}