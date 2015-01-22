package time;

public class Time
{
	public int seconds;
	
	//ex. valid strings "13:57" or "23:58:56"
	public Time(String str)
	{
		String[] timeArr = str.split(":");
		if(timeArr.length == 0)
			throw new IllegalArgumentException();
		
		int hours = 0, minutes = 0, seconds = 0;
		
		if(timeArr.length >= 1)
			hours = Integer.parseInt(timeArr[0]);
		if(timeArr.length >= 2)
			minutes = Integer.parseInt(timeArr[1]);
		if(timeArr.length >= 3)
			seconds = Integer.parseInt(timeArr[2]);
		
		this.seconds = toSeconds(hours, minutes, seconds);
	}
	
	public Time(int hours, int minutes, int seconds)
	{
		this.seconds = toSeconds(hours, minutes, seconds);
	}
	
	public Time(int hours, int minutes)
	{
		this(hours, minutes, 0);
	}
	
	public static int toSeconds(int hours, int minutes, int seconds)
	{
		return hours*60*60 + minutes*60 + seconds;
	}
	
	public int getHours()
	{
		//TODO
		return 0;
	}
	
	public int getMinutes()
	{
		//TODO
		return 0;
	}
	
	public int getSeconds()
	{
		//TODO check if works
		return seconds % 60;
	}
	
	public int getDuration()
	{
		return seconds;
	}
	
	public String toString()
	{
		return getHours()+":"+getMinutes()+":"+getSeconds();
	}
	
	public static void main(String[] args)
	{
		Time t = new Time("23:50:45");
		System.out.println(t.toString());
	}
}