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

for _ in 0..<2022 {
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
}

print(cave.filter { $0 != 0 }.count)
