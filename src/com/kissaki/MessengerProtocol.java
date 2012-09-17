package com.kissaki;

/**
 * ReceiverをOverrideさせるためのabstrctの記述場所
 * 
 * @author sassembla
 * 
 */
public interface MessengerProtocol {
	/*
	 * Javaのインターフェースとしてのreceiver
	 */
	abstract void receiver(String exec, TagValue [] tagValues);
}