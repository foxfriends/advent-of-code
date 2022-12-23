import std/[enumerate,sets,sugar,sequtils,tables]

type Elf = tuple[x: int, y: int]

iterator readelves(): Elf =
    var y = 0
    var line = ""
    while readLine(stdin, line):
        for x, ch in enumerate(line):
            if ch == '#':
                yield (x, y)
        inc(y)

var elves = readelves().toSeq().toHashSet()

type Direction = enum n, s, w, e, ne, se, nw, sw

type Check = tuple[proposed: Direction, checks: array[0..2, Direction]]
type State = array[0..3, Check]

var state: State = [
    (proposed: n, checks: [n, ne, nw]),
    (proposed: s, checks: [s, se, sw]),
    (proposed: w, checks: [w, nw, sw]),
    (proposed: e, checks: [e, ne, se]),
]

proc move(elf: Elf, direction: Direction): Elf =
    case direction
        of n: (elf.x, elf.y - 1)
        of ne: (elf.x + 1, elf.y - 1)
        of nw: (elf.x - 1, elf.y - 1)
        of s: (elf.x, elf.y + 1)
        of se: (elf.x + 1, elf.y + 1)
        of sw: (elf.x - 1, elf.y + 1)
        of e: (elf.x + 1, elf.y)
        of w: (elf.x - 1, elf.y)

proc propose(state: State, elves: HashSet[Elf], elf: Elf): Elf =
    let hasneighbour = toSeq(n..sw)
        .map(d => move(elf, d))
        .any(e => e in elves)
    if not hasneighbour:
        return elf

    for (proposed, checks) in state:
        let conflicts = checks
            .map(d => move(elf, d))
            .any(e => e in elves)
        if not conflicts:
            return move(elf, proposed)
    return elf

var i = 0

while true:
    inc(i)
    let proposals: seq[tuple[elf: Elf, proposed: Elf]] = collect():
        for elf in elves:
            (elf, propose(state, elves, elf))

    if proposals.map(e => e.proposed).toHashSet() == elves:
        break

    var counted = initTable[Elf, int]()
    for (elf, proposal) in proposals:
        if proposal in counted:
            inc(counted[proposal])
        else:
            counted[proposal] = 1

    iterator updated(): Elf =
        for (elf, proposal) in proposals:
            if counted[proposal] == 1:
                yield proposal
            else:
                yield elf
    elves = collect():
        for elf in updated(): {elf}
    let temp = state[0]
    state = [state[1], state[2], state[3], temp]

echo(i)
