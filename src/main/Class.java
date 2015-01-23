package main;

import time.WeekTimes;

public class Class
{
	private String name;
	private String prof;
	private String location;
	private WeekTimes week;
	
	public Class(String name, String prof, String location)
	{
		setName(name);
		setProf(prof);
		setLocation(location);
		week = new WeekTimes();
	}
	
	public void setName(String name)
	{
		this.name = name;
	}
	
	public void setProf(String prof)
	{
		this.prof = prof;
	}
	
	public void setLocation(String location)
	{
		this.location = location;
	}
	
	public int getRoomNumber(String str)
	{
		String numStr = "";
		for(int i = str.length()-1; i > 0; i--)
		{
			if(Character.isDigit(str.charAt(i)))
				numStr = str.charAt(i) + numStr;
			else
				break;
		}
		return Integer.parseInt(numStr);
	}
	
	public void addTimes(String weekTimes)
	{
		//TODO parse the week times string
	}
}
