fun expand(line: String) = when (line) {
    "noop" -> sequenceOf(0)
    else -> sequenceOf(0, line.slice(5 until line.length).toInt())
}

fun main() = generateSequence { readLine() }
    .flatMap { expand(it) }
    .scan(1) { a, b -> a + b }
    .filterIndexed { index, _ -> ((index + 1) - 20) % 40 == 0 } // we want to count as 1-indexed 
    .mapIndexed { index, v -> (20 + index * 40) * v }
    .sum()
    .let { println(it) }
