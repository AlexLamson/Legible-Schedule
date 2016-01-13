package main

import scala.util.control.Breaks._
import java.text.{SimpleDateFormat => DateFormat}
import image._

object Main extends App {
  //parse pasted string
  val classStrings = ParseManager.readInput()
  
  //create class objects
  val classes = classStrings.map { new Class(_) }
//  classes.foreach { c => println(c.toString+"\n") }
  
  //generate image
  val img = ImageManager.generate(classes)
  img.toFile("res/schedule")
}

object ImageManager {
  def generate(classes:List[Class]):Image = {
//    val (w, h) = (1200, 600)
    val (w, h) = (1219, 577)
//    val (w, h) = (1920, 1080)
    
    def dayToX(dayNum:Int) = (1.0*w/7*dayNum).toInt
    def timeToY(time:String) = (TimeManager.toPercent(time)*h).toInt
    
    val img = Image.generate(w, h, g => {
      g.setColor(new Color(230, 230, 230).color)
      g.fillRect(0, 0, w, h)
      
      val earliest = 7
      val latest = 18
      
      //draw hourly sections
      g.setColor(Color.black.color)
      for(hour <- earliest to latest) {
        val time = if(hour < 12) hour+":00AM" else if(hour == 12) "12:00PM" else (hour-12)+":00PM"
        val y = timeToY(time)
        g.drawLine(0, y, w, y)
        if(hour > 7) g.drawString(time, 2, y+12)
      }
      
      //draw weekday breaks
      g.setColor(Color.black.color)
      for(weekday <- 0 to 6) {
        val x = dayToX(weekday)
        g.drawLine(x, 0, x, h)
        
        val avgX = (x + dayToX(weekday+1))/2
        g.drawString(WeekdayManager.numToDay(weekday), avgX-8, 12)
      }
      
      //draw class blocks
      for(c <- classes) {
        for(weekday <- c.weekdays) {
          val weekdayNum = WeekdayManager.dayToNum(weekday)
          val x = dayToX(weekdayNum)
          
          val startPos = (c.timeStartPercent*h).toInt
          val endPos = (c.timeEndPercent*h).toInt
          
          val color = c.toColor()
//          val color = c.toHashColor()
          g.setColor(color.color)
          
          //draw block
          g.fillRect(x, startPos, w/7, endPos-startPos)
          
          //write class name, number and location
          g.setColor(Color.white.color)
          g.drawString(c.name+" "+c.number+" ("+c.classType+")", x+2, startPos+12)
          g.drawString(c.building+" "+c.roomNumber, x+2, startPos+24)
        }
      }
      
    })
    
    img
  }
}

object ParseManager {
  def readInput():List[String] = {
    //read in pasted string
  
    println("paste and press enter 3 times")
    println("====================")
    println()
    
    var emptyCount = 0
    def shouldBreak(ln:String):Boolean = {
      if(ln.isEmpty()) emptyCount += 1
      else emptyCount = 0
      emptyCount == 1
    }
    
    import scala.collection.mutable.ListBuffer
    var lines = ListBuffer[String]()
    
    val linesBuffer = io.Source.stdin.getLines()
    var shouldContinue = true
    while(linesBuffer.hasNext && shouldContinue){
      val ln = linesBuffer.next()
      if(shouldBreak(ln)) shouldContinue = false
      lines += ln
    }
    
    println("====================")
    println("done reading text")
    println()
    
  //  println(lines.mkString("\n"))
    
    val classStrings = lines
      .mkString("\n")
      .replaceAll("Learning Management Systems", "")
      .split("Academic Calendar Deadlines")
      .tail
      .map{_.drop(2).dropRight(1).trim.toString}
      .map{ x=> if(x.charAt(0) == '\n') x.drop(1) else x}
      .toList
    println("Class strings")
    println("--------------------")
//    println(classStrings.mkString("\n\n"))
    
    classStrings
  }
}

object TimeManager {
  val min = toUnix("7:00AM")
  val max = toUnix("6:00PM")
  
  def toUnix(str:String):Int = {
    val format = new DateFormat("hh:mmaa")
    val date = format.parse(str)
    val millisecond = date.getTime()
    (millisecond/1000).toInt
  }
  
  def toPercent(unix:Int):Double = 1.0*(unix-min)/(max-min)
  def toPercent(str:String):Double = toPercent(toUnix(str))
}

object WeekdayManager {
  val weekdays = "Su Mo Tu We Th Fr Sa".split(" ")
  
  def splitWeekdays(str:String):List[String] = {
    str.replaceAll("""([A-Z])""", """ $1""").split(" ").tail.toList
  }
  
  def dayToNum(str:String) = weekdays.indexOf(str)
  
  def numToDay(num:Int) = weekdays(num)
}

class Class(str:String) {
  val (name, number, classType, id, weekdays, timeStart, timeEnd, building, roomNumber) = Class.parse(str)
  
  lazy val timeStartPercent = TimeManager.toPercent(timeStart)
  lazy val timeEndPercent = TimeManager.toPercent(timeEnd)
  
  def toColor():Color = {
    val str = name+" "+number
    val index = Class.classNames.toArray.indexOf(str)
    val hue = 1.0 * index / Class.classNames.size
    
    val mainColor = Color.fromHsv(hue, 0.8, 0.7)
    
    val secondaryColor = {
      classType match {
        case "SEM" => mainColor
        case "LEC" => mainColor
        case "DIS" => new Color(mainColor.color.brighter())
        case "LAB" => new Color(mainColor.color.darker())
      }
    }
    
    secondaryColor
  }
  
  def toHashColor():Color = {
    val mainColor = Color.fromHashedString(name+number)
    
    val secondaryColor = {
      classType match {
        case "SEM" => mainColor
        case "LEC" => mainColor
        case "DIS" => new Color(mainColor.color.brighter())
        case "LAB" => new Color(mainColor.color.darker())
      }
    }
    
    secondaryColor
  }
  
  override def toString() = {
    name+" "+number+"\n"+
    classType+" ("+id+")\n"+
    weekdays.mkString("")+" "+timeStart+" - "+timeEnd+"\n"+
    building+" rm "+roomNumber
  }
}
object Class {
  var classNames = collection.mutable.SortedSet[String]()
  
  def parse(str:String) = {
    val lines = str.split("\n")
    
    val nameNumR = """(.+) ([^\-]+)-?.*""".r
    val nameNumR(name, number) = lines(0)
    
    val typeIdR = """([^\s]+) \(([^\s]+)\)""".r
    val typeIdR(classType, id) = lines(1)
    
    val weekdaysTimeR = """([^\s]+) ([^\s]+) - ([^\s]+)""".r
    val weekdaysTimeR(weekdaysStr, timeStart, timeEnd) = lines(2)
    val weekdays = WeekdayManager.splitWeekdays(weekdaysStr)
    
//    val buildingRoomR = """(.+) (rm|RM|room) ?(.+)""".r
    val buildingRoomR = """^(.+) ro?o?m (.+)$""".r
    val buildingRoomR(building, roomNumber) = lines(3)
//    val (building, roomNumber) = ("","")
    
    //add class to list of classes
    classNames += name+" "+number
    
    (name, number, classType, id, weekdays, timeStart, timeEnd, building, roomNumber)
  }
}
