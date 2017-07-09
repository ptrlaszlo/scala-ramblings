/**
 * Visualizing the k-armed bandit problem with different probabilities of exploration
 */

import $ivy.`org.sameersingh.scalaplot:scalaplot:0.0.4` 

import org.sameersingh.scalaplot.Implicits._ 
import org.sameersingh.scalaplot._

// Number of armed-bandits
val k = 10

val randGen = new scala.util.Random

// Probability of each bandit giving reward
val banditRewardProbs: List[Double] = (1 to k).toList.map(_ => randGen.nextDouble)
// Index of the best bandit
val bestBanditIndex = banditRewardProbs.zipWithIndex.maxBy(_._1)._2

val iterations =  1.0 to 50000.0 by 1.0

// Show reward in graph, or the ratio of best bandit usage
val showReward = false

case class Bandit(avgReward: Double, usedNTimes: Int)

def play(exploreProb: Double, initialGuessedProb: Double): Seq[Double] = {
	// Initialize bandit experience array
	val banditValues: Array[Bandit] = Array.fill(k)(Bandit(initialGuessedProb, 0))

	// Count how many times did we use the best bandit
	var usedTheBest = 0
	var cumulativeReward = 0.0

	iterations.map { iter =>
		// if random value is under explore probability then choose a random bandit, play the best otherwise
		val tryIndex = if (randGen.nextDouble < exploreProb) {
			//explore
			randGen.nextInt(k)
		} else {
			//exploit
			banditValues.zipWithIndex.maxBy(_._1.avgReward)._2
		}

		// try bandit
		val reward = if (randGen.nextDouble < banditRewardProbs(tryIndex)) 1.0 else 0.0

		// Update bandit
		val bandit = banditValues(tryIndex)
		val newavgReward = bandit.avgReward + (reward - bandit.avgReward)/(bandit.usedNTimes + 1)
		banditValues(tryIndex) = Bandit(newavgReward, bandit.usedNTimes + 1)

		// increase the counter if we used the best bandit
		if (bestBanditIndex == tryIndex) usedTheBest = usedTheBest + 1

		// Ratio of best bandit usage
		val ratio = usedTheBest / iter
		cumulativeReward = cumulativeReward + reward

		if (showReward) cumulativeReward else ratio
	}
}

def getYdata(exploreProb: Double, initialGuessedProb: Double) =
	Y(play(exploreProb, initialGuessedProb), label = s"explore prob $exploreProb, init bandit prob $initialGuessedProb")

val data = iterations -> (
	getYdata(exploreProb = 0.001, initialGuessedProb = 1.0),
	getYdata(exploreProb = 0.01, initialGuessedProb = 1.0),
	getYdata(exploreProb = 0.025, initialGuessedProb = 1.0),
	getYdata(exploreProb = 0.1, initialGuessedProb = 1.0),
	getYdata(exploreProb = 0.5, initialGuessedProb = 1.0),
	getYdata(exploreProb = 0.75, initialGuessedProb = 1.0)
)

output(GUI, xyChart(
	data,
	legendPosX = LegendPosX.Center,
    legendPosY = LegendPosY.Bottom,
	showLegend = true))

scala.io.StdIn.readLine()
