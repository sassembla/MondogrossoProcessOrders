package com.kissaki;

import java.util.HashMap;

/**
 * tag-value 
 * 
 * Java互換のためにJavaで書かれている。
 * Scalaだけですむなら別の方法もあるはず。
 * 
 * @author sassembla
 */
public class TagValue extends HashMap<String, Object> {
	public final String m_tag;
	
	private static final long serialVersionUID = 1L;

	/**
	 * value as Object
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, Object value) {
		super();
		super.put(tag, value);
		
		this.m_tag = tag;
	}
	
	/**
	 * value as String
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, String value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	/**
	 * value as int
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, int value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	/**
	 * bool
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, boolean value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	/**
	 * StrArray
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, String [] value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	/**
	 * intArray
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, int [] value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	/**
	 * booleanArray
	 * @param tag
	 * @param value
	 */
	public TagValue(String tag, boolean [] value) {
		super();
		super.put(tag, value);
		this.m_tag = tag;
	}
	
	
	
	/**
	 * タグ取得
	 * @return
	 */
	public String getTag() {
		return m_tag;
	}

	/**
	 * 値
	 * @param tag
	 * @return
	 */
	public Object get(String tag) {
		return super.get(tag);
	}
	
	public String getString(String tag) {
		return (String)super.get(tag);
	}
	
	public int getInt(String tag) {
		return (Integer)super.get(tag);
	}
	
	public boolean getBool(String tag) {
		return (Boolean)super.get(tag);
	}
	
	public String[] getStrArray(String tag) {
		return (String[])super.get(tag);
	}
	
	public int[] getIntArray(String tag) {
		return (int[])super.get(tag);
	}
	
	public boolean[] getBoolArray(String tag) {
		return (boolean[])super.get(tag);
	}
}

