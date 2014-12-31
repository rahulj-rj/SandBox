package sandCastle

object MeanCovariance {
  def main(args: Array[String]): Unit = {
    var x = Array(1.0,2,3,1,1,1,1);
    var y = Array(10,1.0,1,1,1,1,1);
    var n = x.length;
    require (n == y.length);
    
    var (xMean, yMean, sumProd) = dotProdder(x,y);
    var covariance = (sumProd-n*xMean*yMean)/(n-1);
  }
  
  def dotProdder(x:Array[Double], y:Array[Double]) : (Double, Double, Double) = {
    var n = x.length;
    //require(n == y.length);
    
    if (n > 1) {
      var m = math.round((n-1)/2).toInt;
      var (xMean1, yMean1, sumProd1) = dotProdder(x.slice(0,m+1), y.slice(0,m+1));
      var (xMean2, yMean2, sumProd2) = dotProdder(x.slice(m+1,n), y.slice(m+1,n));
      var xMean = (xMean1*(m+1) + xMean2*(n-m-1))/n;
      var yMean = (yMean1*(m+1) + yMean2*(n-m-1))/n;
      return(xMean, yMean, sumProd1+sumProd2);
    } else {
      return(x(0),y(0),x(0)*y(0));
    }
      
    //return (0,0,0);
  }
}