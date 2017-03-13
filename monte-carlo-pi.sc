/**
 * Calculating PI with Monte Carlo method
 */

val rand = scala.util.Random

def calculatePi(eps: Double): (Double, Int) = {
	// Generate points inside circle
	var insideCircle = 0
	// Generated points alltogether
	var count = 0
	var piEstimate = 0.0

	while (Math.abs(Math.PI - piEstimate) > eps) {
	  val x = rand.nextDouble
	  val y = rand.nextDouble

	  if (x*x + y*y <= 1) {
	    insideCircle = insideCircle + 1
	  }

	  count = count + 1
	  piEstimate = 4.0*insideCircle/count
	}
	(piEstimate, count)
}

def runAndPrint(eps: Double) = {
	val r = calculatePi(eps)
	println(f"PI estimate with eps $eps%.6f ${r._1} in ${r._2} tries")
}

runAndPrint(0.01)
runAndPrint(0.001)
runAndPrint(0.0001)
runAndPrint(0.00001)
runAndPrint(0.000001)
