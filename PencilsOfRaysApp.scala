import scala.util.Random

// https://projecteuler.net/problem=372
object PencilsOfRaysApp extends App {
  
  // show points, not efficient
  def findLatticePoints(p: Long, min: Long, max: Long): Seq[(Long, Long)] = {
    // slope min and max
    val delta = 0
    val k0 = Math.sqrt(p - delta)
    val k1 = Math.sqrt(p+1 + delta)
    for{
      x <- min to max
      y <- Math.max(min, k0*x).longValue() to Math.min(max, k1*x).longValue() 
      
      if Math.floor(1.0 * y * y / x / x) == p
    } yield( (x,y) )
  }
  

  
  def countLatticePoints(p: Long, min: Long, max: Long): Long = {
    // slope min and max
    val k0 = Math.sqrt(p)
    val k1 = Math.sqrt(p+1)
    
    var count = 0L;
    val x0 = min
    val x1 = (max/k0).longValue()
    
    var x = x0
    var kx0 = k0*x0
    var kx1 = k1*x0
    while(x <= x1){
      val y0 = Math.max(min, kx0.longValue())
      val y1 = Math.min(max, kx1.longValue())
      var c = y1 - y0 + 1
      if( c > 0 && (y0 * y0 / x / x) != p ) c -= 1;
      if( c > 0 && (y1 * y1 / x / x) != p ) c -= 1;
      
      if(c > 0) {
        count += c
      }
      
      x += 1
      kx0 += k0
      kx1 += k1
      
    }
    
    count
    
  }

  def pencilsOfRays(m0: Long, n: Long): Unit = {
    val t0 = System.nanoTime()
    val m = m0 + 1L
    val k = Math.floor(1.0 * n * n / m / m).longValue()
    val total = Random.shuffle((1L to k by 2L).toList).par.map(countLatticePoints(_, m, n)).sum
    print(s"==> There are ${total} lattice points in [$m0, $n]")
    val t1 = System.nanoTime()
    println("    Elapsed time: " + ((t1 - t0)/1000000) + " ms")
  }
  
  pencilsOfRays(0L, 100L)
  pencilsOfRays(100L, 10000L)
  pencilsOfRays(2000000L, 1000000000L)

}

/* out put:
 *
 * ==> There are 3019 lattice points in [0, 100]    Elapsed time: 88 ms
 * ==> There are 29750422 lattice points in [100, 10000]    Elapsed time: 18 ms
 * ==> There are 301450079531438464 lattice points in [2000000, 1000000000]    Elapsed time: 3389769 ms
 * 
 */
