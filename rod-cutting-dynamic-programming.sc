/**
 * Rod cutting problem
 * What is the optimal way for cutting a rod of length l
 * if prices for i length rods are given in _prices_
 */


// Rod of length 1 has price 1, length 2 has price 5 etc.
val prices = List(0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30)

def getOptimalPrice(l: Int): (Int, List[Int]) = {
	require(l >= 0, "Rod length has to be positive")

	// Optimal prices for lengths given by index
	// Index: the length of rod
	// Tuple._1: optimal price
	// Tuple._2: pieces for optimal price
	var optimal: List[(Int, List[Int])] = List(0 -> List.empty)

	(1 to l).foreach { j =>
		var q: Int = -1
		var steps: List[Int] = List.empty
		(1 to j).foreach { i =>
			val testPrice = prices(i) + optimal(j - i)._1
			if (q < testPrice) {
				q = testPrice
				steps = List(i, j - i)
			}
		}
		optimal = optimal :+ (q, steps)
	}

	optimal(l)
}

def printResult(r: (Int, List[Int])) =
	println(s"\nOptimal price: ${r._1} from ${r._2.filter(_ > 0).sorted.mkString(",")}")

(1 to 10).foreach(l => printResult(getOptimalPrice(l)))
