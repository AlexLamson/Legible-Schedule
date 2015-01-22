package time;

public class WeekTimes
{
	public TimeRange[] week; //sunday = 0
	
	public WeekTimes()
	{
		week = new TimeRange[7];
	}
	
	public WeekTimes(String str)
	{
		String weekDaysStr = str.split(" ")[0];
		String timeRangeStr = str.substring(weekDaysStr.length()+1);
		boolean[] classDays = smallWeekStrToInt(weekDaysStr);
		TimeRange tr = new TimeRange(timeRangeStr);
		
		for(int i = 0; i < 7; i++)
			if(classDays[i])
				week[i] = tr;
	}
	
	public void addTime(int weekDay, TimeRange timeRange)
	{
		week[0] = timeRange;
	}
	
	public void addTime(String weekDayStr, TimeRange timeRange)
	{
		addTime(weekStrToInt(weekDayStr), timeRange);
	}
	
	//ex. valid string "SuMoTuWeThFrSa"
	private boolean[] smallWeekStrToInt(String weekStr)
	{
		boolean[] output = new boolean[7];
		for(int i = 0; i < weekStr.length(); i+=2)
		{
			String smallWeekday = weekStr.charAt(i)+""+weekStr.charAt(i+1);
			output[smallWeekdayStrToInt(smallWeekday)] = true;
		}
		
		return output;
	}
	
	private int smallWeekdayStrToInt(String weekdayStr)
	{
		switch(weekdayStr)
		{
		case "Su": return 0;
		case "Mo": return 1;
		case "Tu": return 2;
		case "We": return 3;
		case "Th": return 4;
		case "Fr": return 5;
		case "Sa": return 6;
		}
		return 0;
	}
	
	private int weekStrToInt(String weekStr)
	{
		switch(weekStr)
		{
		case "Sunday": return 0;
		case "Monday": return 1;
		case "Tuesday": return 2;
		case "Wednesday": return 3;
		case "Thursday": return 4;
		case "Friday": return 5;
		case "Saturday": return 6;
		}
		return 0;
	}
	
	public static void main(String[] args)
	{
		WeekTimes wt = new WeekTimes();
		boolean[] weekArr = wt.smallWeekStrToInt("Su");
		for(boolean b : weekArr)
			System.out.print(b+" ");
		
	}
}
