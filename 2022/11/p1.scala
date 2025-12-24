class Monkey(
    var items: Array[Int],
    val operation: Int => Int,
    val check: Int,
    val success: Int,
    val failure: Int,
) {
    var inspected = 0

    def receive(item: Int) = {
        items = items :+ item
    }

    def turn(monkeys: List[Monkey]) = {
        inspected += items.length
        for (item <- items) {
            val updated = operation(item) / 3
            monkeys(if (updated % check == 0) success else failure) receive updated
        }
        items = Array()
    }
}

object P1 {
    def main(args: Array[String]) = {
        val monkeys = List(
            Monkey(Array(84, 72, 58, 51)                , _ * 3     , 13, 1, 7),
            Monkey(Array(88, 58, 58)                    , _ + 8     , 2 , 7, 5),
            Monkey(Array(93, 82, 71, 77, 83, 53, 71, 89), v => v * v, 7 , 3, 4),
            Monkey(Array(81, 68, 65, 81, 73, 77, 96)    , _ + 2     , 17, 4, 6),
            Monkey(Array(75, 80, 50, 73, 88)            , _ + 3     , 5 , 6, 0),
            Monkey(Array(59, 72, 99, 87, 91, 81)        , _ * 17    , 11, 2, 3),
            Monkey(Array(86, 69)                        , _ + 6     , 3 , 1, 0),
            Monkey(Array(91)                            , _ + 1     , 19, 2, 5),
        )

        for (i <- 1 to 20) {
            monkeys.foreach(_.turn(monkeys))
        }

        println(monkeys.map(_.inspected).sorted.takeRight(2).reduceLeft(_ * _))
    }
}
