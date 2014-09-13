package sandCastle;

object PCA {
  def main(args: Array[String]): Unit = {
    val fileName = "C:\\Users\\rahul\\Desktop\\dataSet.txt";
	var (y,x) = Math101.file2array(fileName);
	var (vals, vecs) = eigenizer(x);
  }
  
  def eigenizer(x: Array[Array[Double]]): (Array[Double], Array[Array[Double]]) = {
    val n = x(0).length;
    val m = x.length;
    
    //var mean : Array[Double] = Array.fill(n)(0.0);
	//var varCov : Array[Array[Double]] = Array.fill(n)(Array.fill(n)(0));
	var mean = Array.ofDim[Double](n);
	var varCov = Array.ofDim[Double](n, n);
	
	var i = 0;
	var j = 0;
	var k = 0;
	var temp = 0.0;
	
	var x1Temp = Array.ofDim[Double](m);
	var x2Temp = Array.ofDim[Double](m);
	var covariance = 0.0;
	
	for (i <- n-1 to 0 by -1) {
	  for (j <- n-1 to i by -1) {
	    for (k <- 0 to m-1) {
	      x1Temp(k) = x(k)(i);
	      x2Temp(k) = x(k)(j);
	    }
	    var (x1Mean, x2Mean, sumProd) = MeanCovariance.dotProdder(x1Temp, x2Temp);
	    mean(i) = x1Mean;
	    varCov(i)(j)  = (sumProd-m*x1Mean*x2Mean)/(m-1);
	    //println(varCov(i)(j));
	  }
	  println(mean(i));
	}
	
	var eigVals = Array.ofDim[Double](n);
	var eigVecs = Array.ofDim[Double](n, n);
	
    return (eigVals, eigVecs); 
  }
}