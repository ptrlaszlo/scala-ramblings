/**
 * Solving tower of hanoi with binary counting
 * Steps:
 * Incrementing a counter from 1 in each step.
 * Always moving the n-th disk,
 * where n is the index of the "smallest" '1' bit in the incremented numbers' binary form 
 */

case class Rod(disks: List[Int] = List.empty) {
	def add(disk: Int): Rod = disks match {
		case Nil => Rod(List(disk))
		case h :: _ if h > disk => Rod(disk :: disks)
		case _ => throw new Exception(s"Can't add disk $disk to $disks")
	}

	def removeTop: Rod = Rod(disks.tail)

	override def toString = if (disks.isEmpty) "-" else disks.mkString(" ")
}

val fullStack = List(0, 1, 2, 3, 4)

var rods = List(Rod(fullStack), Rod(), Rod())
var counter = 1


println(s"Initial state: ${rods.mkString(", ")}")
while (!rods.tail.exists(_.disks == fullStack)) {
	val indexOfDiskToMove = counter.toBinaryString.reverse.indexOf('1')
	if (indexOfDiskToMove == 0) {
		rods = moveZeroDisk(rods)
	} else {
		rods = moveNonZeroDisk(indexOfDiskToMove, rods)
	}
	println(s"Step $counter: Moving disk $indexOfDiskToMove Rods: ${rods.mkString(", ")}")
	counter = counter + 1
}

// Move the zero disk to the next rod (in cyclic manner)
def moveZeroDisk(rods: List[Rod]): List[Rod] = {
	val rodWithZeroDisk = rods.indexWhere(_.disks.headOption.exists(_ == 0))
	require(rodWithZeroDisk >= 0, "No 0 disk found")
	val rodToMoveZeroDisk = (rodWithZeroDisk + 1) % rods.size
	rods.zipWithIndex.map { case (rod, index) =>
		if (index == rodWithZeroDisk)
			rod.removeTop
		else if (index == rodToMoveZeroDisk)
			rod.add(0)
		else
			rod
	}
}

// For 3 rods there is only one place to move nonzero disks - not it's current position and not where zero is
def moveNonZeroDisk(disk: Int, rods: List[Rod]): List[Rod] = {
	require(disk != 0, "Function for moving nonzero disks")
	rods.map { rod =>
		if (rod.disks.headOption.exists(_ == disk)) // remove the disk from it's current place
			rod.removeTop
		else if (rod.disks.isEmpty || rod.disks.headOption.exists(_ > disk)) // add it to the new place
			rod.add(disk)
		else 
			rod
	}
}
