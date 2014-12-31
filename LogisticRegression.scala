package sandCastle;

object LogisticRegression {
  def main(args: Array[String]): Unit = {
    //learning rate and iter
    val alpha : Double = 0.01;
    val iter : Int = 1000;
    
    val fileName = "C:\\Users\\rahul\\Desktop\\dataSet.txt";
	var (y,x) = Math101.file2array(fileName);
	
	//initialize theta
	var theta : Array[Double] = Array.ofDim[Double](x(0).length);
    
    val (costF, thetaUpd) = gradientDesc(x, y, theta, alpha, iter);
    
    costF.foreach{println}
    println("done");
    thetaUpd.foreach{println}
    
  } 
  
  def gradientDesc(x: Array[Array[Double]], y: Array[Double], theta: Array[Double], alpha: Double, iter: Int): (Array[Double], Array[Double]) = {
    val m : Int = y.length;
    val n : Int = x(0).length;
    require(m==x.length);
    require(n==theta.length);
    
    //initialize learning rate and iterations
    //val alpha : Double = 0.5;
    //val iter : Int = 10;
    
    var J = new Array[Double](iter);
    var h = new Array[Double](m);
    var gradStep = new Array[Double](n);
    var tempDot : Double = 0;
    
    var i : Int = 0;
    var j : Int = 0;
    var k : Int = 0;
    
    for (k <- 0 to iter - 1) {
      for (i <- 0 to m-1) {
        for (j <- 0 to n-1) {
          //println("i " + i+ " j " + j+ " k " + k);
          tempDot += theta(j)*x(i)(j);
        }
        //calc h_theta
        h(i) = 1/(1+math.exp(-tempDot));
        //println("hdone");
        
        //cost function	
        J(k) -= (y(i) * math.log(h(i)) + (1-y(i)) * math.log(1-h(i))) / m;
        //println("Jdone");
        
        //gradient step
        for (j <- 0 to n-1) {
        	gradStep(j) += alpha*(h(i)-y(i))*x(i)(j);
        }
        //println("graddone");
        
        tempDot = 0;
      }
      
      //theta update
      for (j <- 0 to n-1) {
        theta(j) -= gradStep(j);
      }
      
    }
    return (J, theta);
  }

  //looks similar to gradient descent but this calculates only cost function, doesn't iterate
  def costFunc(x: Array[Array[Double]], y: Array[Double], theta: Array[Double]): (Double, Array[Double]) = {
    val m : Int = y.length;
    val n : Int = x(0).length;
    require(m==x.length && n==theta.length);
    
    var J : Double = 0;
    var h : Array[Double] = Array[Double](m);
    var tempDot : Double = 0;
    
    var i : Int = 0;
    var j : Int = 0;

    for (i <- 0 to m-1) {
      for (j <- 0 to n-1) {
        tempDot += theta(j)*x(i)(j);
      }
      //calc h_theta
      h(i) = 1/(1+math.exp(-tempDot));
      //cost function
      J -= (y(i) * math.log(h(i)) + (1-y(i)) * math.log(1-h(i)))/m;
      
      tempDot = 0;
    }
    
    return (J, h);
  }
}