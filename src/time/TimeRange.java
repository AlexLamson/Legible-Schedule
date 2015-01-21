package time;

import java.sql.Time;

public class TimeRange
{
	public Time start, stop;
	
	public TimeRange(Time start, Time stop)
	{
		this.start = start;
		this.stop = stop;
	}
	
	public TimeRange(String start, String stop)
	{
		//TODO
	}
}
