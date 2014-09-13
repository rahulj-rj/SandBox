package sandCastle;

import scala.collection.immutable.Map

object Math101 {
  def main(args: Array[String]): Unit = {
    
    val fileName = "C:\\Users\\rahul\\Desktop\\dataSet.txt";
    var (y,x) = file2array(fileName);
    
    /*var x = ArrayBuffer.fill[Double](5)(2);
    var y = 0.0;
    for (i <- 0 to x.length-1) {
      y = x.reduceLeft(_ + _);
      println(y);
      println(x(i));
    }*/
    
    /*var myVar : Double = 1+2;
    println(myVar); 
    
    var myNums : Array[Array[Int]] = Array(Array(1,2,3,4), Array(2,3,4,5));
    var Array(1,2,3) :* myNums(0);
    myNums = myNums.map(_*2);
    require(myNums.forall(_.length==4));
    
    println(myNums(0).length);*/
    
  }
  
  def file2array(fileName: String): (Array[Double], Array[Array[Double]]) = {
    //read file
    //val fileName = "C:\\Users\\rahul\\Desktop\\dataSet.txt";
    
    var lines = io.Source.fromFile(fileName).getLines().toArray[String];
    //val lines = fileLines.filterNot(_.isEmpty).map { line => (line.toList).filter(e => e != ' ')}.toArray
    var lineOne = lines(0).split("\\s+");
    var x = Array.ofDim[Double](lines.length, lineOne.length-1);
    var y = Array.ofDim[Double](lines.length);
    
    var temp = Array.ofDim[String](lineOne.length);
    var i = 0;
    var j = 0;
	for (i <- 0 to lines.length-1) {
	  temp = lines(i).split("\\s+");
	  y(i) = temp(0).toDouble;
	  for (j <- 1 to temp.length - 1) {
	    x(i)(j-1) = temp(j).toDouble;
	    //println(x(i)(j-1),lines.length,temp.length, i,j, lines(i));
	  }
    }
	return (y,x);
  }
}


