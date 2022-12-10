fun expand(line: String) = when (line) {
    "noop" -> sequenceOf(0)
    else -> sequenceOf(0, line.slice(5 until line.length).toInt())
}

fun main() = generateSequence { readLine() }
    .flatMap { expand(it) }
    .scan(1) { a, b -> a + b }
    .mapIndexed { index, v -> if (((index % 40) - v) in -1..1) { "#" } else { " " } }
    .chunked(40)
    .forEach { println(it.joinToString("")) }
