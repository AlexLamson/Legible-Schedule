/*
 * Represents an image as a 2d array of Colors
 * 
 */

package image

import scala.collection.JavaConversions._
import java.awt.image.{BufferedImage => BImage}
import java.awt.Graphics

//get should return Option[Color]
//rename the current get to getMod
//replace bad instances of getMod with get().get
//filter should return a list of Pixels

class Image(val img:BImage) extends Iterable[Color]{
  def this(w:Int, h:Int) = this(new BImage(w, h, BImage.TYPE_INT_RGB))
  def this(path:String) = this(File.readBImage(path))
  
  def width = img.getWidth
  def height = img.getHeight
  
  //return the color of a pixel at a given location
  def apply(x:Int, y:Int):Option[Color] = get(x, y)
  def getMod(x:Int, y:Int): Color = 
    new Color(img.getRGB( mod(x, width), mod(y, height) ))
  def get(x:Int, y:Int):Option[Color] = {
    if(inBounds(x, y)) Some(new Color(img.getRGB( x, y )))
    else None
  }
  def get(pos:(Int, Int)):Option[Color] = {
    if(inBounds(pos._1, pos._2)) Some(new Color(img.getRGB( pos._1, pos._2 )))
    else None
  }
  def getPixel(x:Int, y:Int):Option[Pixel] = {
    val possiblePos = get(x, y)
    if(possiblePos.nonEmpty) {
      Some(new Pixel(possiblePos.get, (x, y)))
    }
    else {
      None
    }
  }
  
  //syntax: someImage(someX, someY) = someColor
  //try not to use this method, use recolor instead
  def update(x:Int, y:Int, color:Color):Image = update(x, y, color.rgb)
  def update(x:Int, y:Int, rgb:Int):Image = {
    val copyImg = copyBImage
    copyImg.setRGB(x, y, rgb)
    new Image(copyImg)
  }
  
  //returns true if the given point is within the bounds of the image
  def inBounds(x:Int, y:Int):Boolean = (0 <= x && x < width) && (0 <= y && y < height)
  
  //apply color change to all pixels and return the resulting image
  def recolor(f:(Pixel)=>Color):Image = {
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, f(new Pixel(x, y, get(x, y).get)).rgb)
    new Image(newImg)
  }
  
  //return list of all colors in image which `f` returns true on
  def filter(f: (Pixel) => Boolean):List[Color] = {
    val colors = for{
      y <- 0 to height-1
      x <- 0 to width-1
      c = get(x, y).get
      if(f(new Pixel(x, y, c)))
    } yield c
    colors.toList
  }
  
  //get the position of the first pixel with the given color
  def find(c:Color):Option[(Int, Int)] = {
    val pixel = iteratorPixels.find{ p => (p.color == c) }
    pixel match{
      case None => None
      case Some(p:Pixel) => Some(p.x, p.y)
    }
  }
  
  //https://www.youtube.com/watch?v=Sh2TrCYXdvo
  def convolve(kernel:Array[Array[Double]], f:(Color)=>Float=_.v):Array[Array[Int]] = {
    //ensure the kernel is of a usable size
    assert(kernel.length >= 1, "kernel must have positive size")
    assert(kernel.length == kernel(0).length, "kernel must be square")
    assert(kernel.length % 2 != 0, "kernel must have odd width")
    
    val kHeight = kernel.length
    val kWidth = kernel(0).length
    
    def bound(low:Int, x:Int,high:Int) = {
      if(x < low) low
      else if(x > high) high
      else x
    }
    
    def getColorExtend(x:Int, y:Int):Color = 
      get(bound(0, x, width-1), bound(0, y, height-1)).get
    
    val arr = Array.fill(height, width)(0)
    
    for(y <- 0 to height-1; x <- 0 to width-1) {
      var sum = 0.0
      
      for(kernelY <- 0 to kHeight-1; kernelX <- 0 to kWidth-1) {
        val imageX = x - kWidth/2 + kernelX
        val imageY = y - kHeight/2 + kernelY
        sum += f(getColorExtend(imageX, imageY)) * kernel(kernelX)(kernelY)
      }
      
      arr(y)(x) = bound(0, (sum*255).toInt, 255)
    }
    
    arr
  }
  
  type Kernel = Array[Array[Double]]
  type IntArray = Array[Array[Int]]
  def convolve(rKernel:Kernel, gKernel:Kernel, bKernel:Kernel):(IntArray, IntArray, IntArray) = {
    (convolve(rKernel, _.r), convolve(gKernel, _.g), convolve(bKernel, _.b))
  }
  def convolve(kernelArr:Array[Kernel]):(IntArray, IntArray, IntArray) = {
    assert(kernelArr.length == 3)
    (convolve(kernelArr(0), _.r), convolve(kernelArr(1), _.g), convolve(kernelArr(2), _.b))
  }
  
  //DEBUG
  def convolveCray(rKernel:Kernel, gKernel:Kernel, bKernel:Kernel):(IntArray, IntArray, IntArray) = {
    (convolve(rKernel, _.v), convolve(gKernel, _.v), convolve(bKernel, _.v))
  }
  
  //iterate through all the colors in the image
  def iterator = new Iterator[Color] {
    val iter = iteratorPositions
    def hasNext = iter.hasNext
    def next = get(iter.next).get
  }
  
  //iterate through all the pixels (position and color) in the image
  def iteratorPixels = new Iterator[Pixel] {
    val iter = iteratorPositions
    def hasNext = iter.hasNext
    def next = {
      val pos = iter.next
      new Pixel(pos, get(pos).get)
    }
  }
  
  //iterate through all the pixels (position) in the image
  def iteratorPositions = new Iterator[(Int, Int)] {
    var (x, y) = (0, 0)
    def hasNext = x < width-1 || y < height-1
    def next = {
      x = x + 1
      if(x == width){ x = 0; y = y + 1 }
      (x, y)
    }
  }
  
  def +(that:Image):Image = {
    assert(this.width == that.width, "cannot sum images, widths are inequal")
    assert(this.height == that.height, "cannot sum images, heights are inequal")
    
    val colors = this.iterator.zip(that.iterator).map{case (c1, c2) => c1+c2}
    
    Image.fromColorList(colors.toList, width, height)
  }
  
  //return the brightest pixel
  def brightest:Color = 
    iterator.foldLeft(Color.black)( (a:Color, b:Color) => Color.chooseBrighter(a, b) )
  
  //make all 3 rgb components equal for all pixels
  def toGrayscale:Image = this.recolor{ x => Color.fromProbToGray(x.color.v) }
  
  //average all the colors in the image
  def average:Color = Color.average(iterator.toList)
  
  //create a histogram of color values in the image
  //f can be specified to use some other attribute of each color
  //c can be specified to render the bars with a given color
  def histogramValues(f:(Color)=>Int=(x)=>(x.v*255).toInt, c:Color=Color.white):Image = {
    val countsMap = iterator.toList.groupBy { f }.mapValues { _.size }
    val counts = (0 until 255).map{ i => countsMap.getOrElse(i, 0) }.toArray
    Image.histogram(counts, c)
  }
  
  //sum the histograms for each color component to
  //create a histogram which displays all 3 histograms at once
  def histogramRGB():Image = {
    val histR = histogramValues(_.r, Color.red)
    val histG = histogramValues(_.g, Color.green)
    val histB = histogramValues(_.b, Color.blue)
    (histR + histG + histB)
  }
  
  //invert all the colors in the image
  def invert():Image = recolor{ x:Pixel => x.color.invert() }
  
  //grow/shrink an image so it has exactly the given width and height
  def scaleBy(factor:Double): Image = {
    val (w, h) = ((width*factor).toInt, (height*factor).toInt)
    var newImg = new BImage(w, h, BImage.TYPE_INT_RGB)
    for(y <- 0 to h-1; x <- 0 to w-1)
      newImg.setRGB(x, y, get(width*x/w, height*y/h).get.rgb)
    new Image(newImg)
  }
  
  //grow/shrink an image so it has exactly the given width and height
  def scale(w:Int, h:Int): Image = {
    var newImg = new BImage(w, h, BImage.TYPE_INT_RGB)
    for(y <- 0 to h-1; x <- 0 to w-1)
      newImg.setRGB(x, y, get(width*x/w, height*y/h).get.rgb)
    new Image(newImg)
  }
  
  //shrink an image so it fits inside the rectangle maxWidth, maxHeight
  def constrainTo(maxWidth:Int, maxHeight:Int): Image = {
    val w = math.min(width, maxWidth)
    val newHeight = 1d*w/width*height
    val hFinal = math.min(newHeight, maxHeight)
    val wFinal = 1d*hFinal/height*width
    
    scale(wFinal.toInt, hFinal.toInt)
  }
  
  //create a new image out of a window of the image
  def crop(x:Int, y:Int, w:Int, h:Int) = new Image(img.getSubimage(x, y, w, h))
  
  //return a new image which is equivalent to the image
  def copy:Image = new Image(copyBImage)
  
  //return a new BufferedImage which is equivalent to the one describing the image
  def copyBImage = {
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, get(x,y).get.rgb)
    newImg
  }
  
  def g = img.getGraphics
  
  private def mod(a:Int, b:Int) = { val c = a % b; if(c < 0) c + b else c }
  
  //converts each pixel into an int of the range [0,255] according to its value
  def to2dIntArray() = {
    def getRow(y:Int) = (for{ x <- 0 to width-1 } yield (get(x, y).get.v*255).toInt).toArray
    (for{ y <- 0 to height-1 } yield getRow(y)).toArray
  }
  
  //converts each pixel into an float of the range [0,1] according to its value
  def to2dFloatArray():Array[Array[Float]] = {
    def getRow(y:Int) = (for{ x <- 0 to width-1 } yield get(x, y).get.v).toArray
    (for{ y <- 0 to height-1 } yield getRow(y)).toArray
  }
  
  //write the image to a file with the given filename
  //ex usage: myImage.toFile("some_filename")
  def toFile(filename:String) = File.toFile(filename, this)
  
  override def toString = "Image("+width+", "+height+")"
}
object Image{
  
  //draw a histogram image given an array of bin counts
  def histogram(counts:Array[Int], c:Color=Color.white) = {
    val max = counts.max
    val outputImg = new Image(512, 512)
    var g = outputImg.g
    g.setColor(c.color)
    for((count, i) <- counts.zipWithIndex) {
      val normalized = (1.0*count/max*512).toInt
      g.drawLine(i*2, 511, i*2, 511-normalized)
      g.drawLine(i*2+1, 511, i*2+1, 511-normalized)
    }
    outputImg
  }
  
  //draw a scatterplot image given an list of points
  def plotPoints(points:List[(Double, Double)],
      smallPoints:Boolean=true, drawLine:Boolean=false) = {
    
    assert(points.size != 0, "Image.plotPoints problem, empty list provided")
    
    val width = 512
    val height = 512
    
    val minX = points.minBy(_._1)._1
    val minY = points.minBy(_._2)._2
    val maxX = points.maxBy(_._1)._1
    val maxY = points.maxBy(_._2)._2
    
    def normalizeX(x:Double) = {
      val percentage = 1.0*(x-minX)/(maxX-minX)
      val output = (percentage*(width-1-0)+0).toInt
      output
    }

    def normalizeY(y:Double) = {
      val percentage = 1.0*(y-minY)/(maxY-minY)
      val output = ((1.0-percentage)*(height-1-0)+0).toInt
      output
    }
    
    val normalizedPoints = points.map{ x =>  (normalizeX(x._1), normalizeY(x._2))}
    
    val outputImg = new Image(512, 512)
    var g = outputImg.g
    g.setColor(Color.white.color)
    
    for((x, y) <- normalizedPoints) {
      if(smallPoints) outputImg.img.setRGB(x, y, Color.white.rgb)
      
      val w = 4
      val h = 4
      if(!smallPoints) {
        g.setColor(Color.white.color)
        g.drawOval(x-w/2, y-h/2, w, h)
      }
    }
    
    if(drawLine) {
      val pairs = normalizedPoints.dropRight(1).zip(normalizedPoints.tail)
      for(((x1, y1), (x2, y2)) <- pairs) {
        g.setColor(Color.white.color)
        g.drawLine(x1, y1, x2, y2)
      }
    }
    
    outputImg
  }
  
  //make image of the specified size by applying the given function to every pixel location
  def generate(width:Int, height:Int, f:(Int, Int) => Color):Image = 
    new Image(width, height).recolor{(p:Pixel) => f(p.x, p.y)}

  //make image of the specified size by applying the given function to every pixel location
  def generate(width:Int, height:Int, f:(Graphics)=>Unit):Image = {
    val img = new Image(width, height)
    f(img.g)
    img
  }
  
  //load an image from a file
  def fromFile(filename:String) = File.readImage(filename)
  
  //convert a 2d array of ints to a grayscale image
  def from2dIntArray(arr:Array[Array[Int]], f:(Int)=>Color=Color.fromProbToGray(_)):Image = {
    val height = arr.length
    val width = arr(0).length
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1){
      val num = arr(y)(x)
      newImg.setRGB(x, y, f(num).rgb)
    }
    new Image(newImg)
  }
  
  //convert a 2d array of floats to a grayscale image
  def from2dFloatArray(arr:Array[Array[Float]], f:(Float)=>Color=Color.fromProbToGray(_)):Image = {
    val width = arr(0).length
    val height = arr.length
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB);
    
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, Color.fromProbToGray(arr(y)(x)).rgb)
    
    new Image(newImg)
  }
  
  //convert a 3 2d arrays of ints to a color image
  type IntArray = Array[Array[Int]]
  def from2dIntArrays(rArr:IntArray, gArr:IntArray, bArr:IntArray):Image = {
    val height = rArr.length
    val width = rArr(0).length
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1){
      val r = rArr(y)(x)
      val g = gArr(y)(x)
      val b = bArr(y)(x)
      newImg.setRGB(x, y, new Color(r, g, b).rgb)
    }
    new Image(newImg)
  }
  
  //create an image of size w x h from a list of colors
  //if w and h are not specified, the image will be aprox. square
  def fromColorList(colors:List[Color], w:Int=0, h:Int=0):Image = {
    
    val width = if(w == 0) Math.sqrt(colors.length).ceil.toInt else w
    val height = if(h == 0) width else h
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB);
    val iterator = colors.iterator
    
    for(y <- 0 to height-1; x <- 0 to width-1){
        if(iterator.hasNext) newImg.setRGB(x, y, iterator.next.rgb)
        else newImg.setRGB(x, y, Color.white.rgb)
    }
    
    new Image(newImg)
  }
  
  //random black and white pixels
  def whiteNoise(width:Int, height:Int):Image = {
    def randColor(x:Int, y:Int):Color = {
      val randInt = if(math.random > 0.5) 0 else 255
      new Color(randInt, randInt, randInt)
    }
    generate(width, height, randColor(_, _))
  }
  
  //random gray pixels
  def grayNoise(width:Int, height:Int):Image = {
    def randColor(x:Int, y:Int):Color = {
      val randInt = (math.random*255).toInt 
      new Color(randInt, randInt, randInt)
    }
    generate(width, height, randColor(_, _))
  }
  
  //make a square image of random interpolated black and white squares
  def noise(powOf2:Int):Image = {
    def powerOf2(n:Int) = 1 << n
    
    val width = powerOf2(powOf2) //2^powOf2
    val height = width
    val noise = whiteNoise(width, height).to2dFloatArray
    
    def interpolate(pos1:Int, pos2:Int, posMid:Int, val1:Float, val2:Float):Float = {
      val percent = 1f*(posMid-pos1)/pos2
      percent*(val2-val1) + val1
    }
    
    def getSamplePositions(x:Int, width:Int, pow:Int):(Int, Int) = {
      val sampleSize = (width/pow)
      val samplePos1 = (x/sampleSize)*sampleSize
      val samplePos2 = (samplePos1 + sampleSize)
      (samplePos1, samplePos2)
    }
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(x <- 0 to width-1){
      for(y <- 0 to height-1){
        
        val samples = for{
          i <- 1 to powOf2
          pow = powerOf2(i)
          
          (x1, x2) = getSamplePositions(x, width, pow)
          (y1, y2) = getSamplePositions(y, height, pow)
          
          xMod = x2 % width
          yMod = y2 % height
          
          xVal = interpolate(x1, x2, x, noise(y)(x1), noise(y)(xMod))
          yVal = interpolate(y1, y2, y, noise(y1)(x), noise(yMod)(x))
          
          prob = (xVal+yVal)/2f
          
          //
          
          sampleXPos = (x/(width/pow))*(width/pow)
          sampleYPos = (y/(height/pow))*(height/pow)
//        } yield noise(sampleYPos)(sampleXPos)
        } yield prob
        
        val avg = samples.sum/samples.length
        
        newImg.setRGB(x, y, Color.fromProbToGray(avg).rgb)
//        newImg.setRGB(x, y, Color.fromProbToHeat(avg).rgb)
      }
    }
    
    new Image(newImg)
  }
  
  def noiseLayers(powOf2:Int):List[Image] = {
    def powerOf2(n:Int) = 1 << n
    
    val width = powerOf2(powOf2) //2^powOf2
    val height = width
    val noise = whiteNoise(width, height).to2dFloatArray
    
    def interpolate(pos1:Int, pos2:Int, posMid:Int, val1:Float, val2:Float):Float = {
      val percent = 1f*(posMid-pos1)/pos2
      percent*(val2-val1) + val1
    }
    
    def getSamplePositions(x:Int, width:Int, pow:Int):(Int, Int) = {
      val sampleSize = (width/pow)
      val samplePos1 = (x/sampleSize)*sampleSize
      val samplePos2 = (samplePos1 + sampleSize)
      (samplePos1, samplePos2)
    }
    
    var images = new Array[BImage](powOf2)
    for(i <- 0 to powOf2-1)
      images(i) = new BImage(width, height, BImage.TYPE_INT_RGB)
    
    for(x <- 0 to width-1){
      for(y <- 0 to height-1){
        
        val samples = for(i <- 1 to powOf2){
          val pow = powerOf2(i)
          
          val (x1, x2) = getSamplePositions(x, width, pow)
          val (y1, y2) = getSamplePositions(y, height, pow)
          
          val xMod = x2 % width
          val yMod = y2 % height
          
          val xVal = interpolate(x1, x2, x, noise(y)(x1), noise(y)(xMod))
          val yVal = interpolate(y1, y2, y, noise(y1)(x), noise(yMod)(x))
          
//          val prob = (xVal+yVal)/2f
          val prob = noise(x1)(y1)
          
          //
          
          val sampleXPos = (x/(width/pow))*(width/pow)
          val sampleYPos = (y/(height/pow))*(height/pow)
        
          
          images(i-1).setRGB(x, y, Color.fromProbToGray(prob).rgb)
          
//        newImg.setRGB(x, y, Color.fromProbToHeat(avg).rgb)
        }
      }
    }
    
    images.map(new Image(_)).toList
  }
}

object Kernel {
  def make2D(s:String):Array[Array[Double]] = {
    s.replaceAll("; ",";").split(";").toArray.map{_.split(" ").map{_.toDouble}}
  }
  
  def generateKernel(width:Int, f:(Int, Int)=>Double):Array[Array[Double]] = {
    val height = width
    val arr = Array.fill(width, height)(1.0)
    for(y <- 0 to width-1) {
      for(x <- 0 to width-1) {
        arr(x)(y) = f(x, y)
      }
    }
    arr
  }
}