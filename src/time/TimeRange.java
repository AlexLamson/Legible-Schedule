package time;

public class TimeRange
{
	public Time start, stop;
	
	public TimeRange(Time start, Time stop)
	{
		this.start = start;
		this.stop = stop;
	}
	
	public TimeRange(String startStr, String stopStr)
	{
		this.start = new Time(startStr);
		this.stop = new Time(stopStr);
	}
	
	//ex. valid strings "13:57 - 14:00"
	public TimeRange(String timeRangeStr)
	{
		String[] timeRangeArr = timeRangeStr.split("-");
		
		start = new Time(timeRangeArr[0].trim());
		stop = new Time(timeRangeArr[1].trim());
	}
	
	public int getDuration()
	{
		return start.getTimeBetween(stop);
	}
	
	public double getPercentageOfDay()
	{
		return 1.0*getDuration()/(24*60*60);
	}
}
