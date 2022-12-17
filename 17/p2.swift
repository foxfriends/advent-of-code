let wind = Array(readLine()!)
let shapes = [
    [0b0011110],
    [0b0001000, 0b0011100, 0b0001000],
    [0b0011100, 0b0000100, 0b0000100],
    [0b0010000, 0b0010000, 0b0010000, 0b0010000],
    [0b0011000, 0b0011000],
]

var windindex = 0
var shapeindex = 0

var cave: [Int] = []
cave.reserveCapacity(5000)

func collides(_ shape: [Int], in cave: [Int], at y: Int) -> Bool {
    return shape
        .enumerated()
        .contains { (i, row) in cave[y + i] & row != 0 }
}

func surface(_ cave: [Int]) -> [Int] {
    var collect = 0
    for (i, line) in cave.enumerated().reversed() {
        collect |= line
        if collect == 0b1111111 {
            return Array(cave[i..<cave.filter { $0 != 0 }.count])
        }
    }
    return cave
}

struct State: Equatable, Hashable {
    let windindex: Int
    let shapeindex: Int
    let lines: [Int]
}

var seenStates: [State: (height: Int, settled: Int)] = [:]

var offset = 0
var settled = 0
let target = 1000000000000
while settled < target {
    var shape = shapes[shapeindex]
    while cave.suffix(3 + shape.count).filter({ $0 == 0 }).count != 3 + shape.count { cave.append(0) }
    var y = cave.firstIndex { $0 == 0 }! + 3
    shapeindex = (shapeindex + 1) % shapes.count

    while true {
        let direction = wind[windindex]
        windindex = (windindex + 1) % wind.count

        let againstWall = direction == "<"
            ? shape.contains { $0 & 0b1000000 != 0 }
            : shape.contains { $0 & 0b0000001 != 0 }
        if !againstWall {
            let movedshape = direction == "<"
                ? shape.map { $0 << 1 }
                : shape.map { $0 >> 1 }
            if !collides(movedshape, in: cave, at: y) { shape = movedshape }
        }
        if y > 0 && !collides(shape, in: cave, at: y - 1) {
            y -= 1
        } else {
            for i in 0..<shape.count {
                cave[y + i] |= shape[i]
            }
            break
        }
    }

    settled += 1

    if offset == 0 {
        let lines = surface(cave)
        let state = State(windindex: windindex, shapeindex: shapeindex, lines: lines)
        if let seen = seenStates[state] {
            let height = cave.filter { $0 > 0 }.count
            let blocksbetween = settled - seen.settled
            let heightbetween = height - seen.height
            let timesfit = (target - settled) / blocksbetween

            offset = timesfit * heightbetween
            settled += timesfit * blocksbetween
        }
        seenStates[state] = (height: cave.lastIndex { $0 != 0 }! + 1, settled: settled)
    }
}

print(offset + cave.filter { $0 > 0 }.count)
