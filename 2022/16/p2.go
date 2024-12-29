package main

import (
    "strings"
    "sort"
    "bufio"
    "os"
    "fmt"
    "regexp"
    "strconv"
)

type valve struct {
    flowrate int
    tunnels []string
}

func addToPath(pathStr, valve string) string {
    path := append(strings.Split(pathStr, ","), valve)
    sort.Strings(path)
    return strings.Join(path, ",")
}

func removeFromPath(pathStr, valve string) string {
    path := strings.Split(pathStr, ",")
    sort.Strings(path)
    return strings.Join(path, ",")
}

type state []string

func parseState(s string) state {
    if s == "" { return make(state, 0) }
    return strings.Split(s, ",")
}

func (s state) path() string {
    sort.Strings(s)
    return strings.Join(s, ",")
}

func (s state) open(v string) state {
    s = append(s, v)
    return s
}

type position struct {
    me, el string
}

func makeposition(me, el string) position {
    if me <= el {
        return position { me, el }
    } else {
        return position { el, me }
    }
}

func cross(mes, els []string) []position {
    positions := make([]position, 0, len(mes) * len(els))
    for _, me := range mes {
        for _, el := range els {
            positions = append(positions, makeposition(me, el))
        }
    }
    return positions
}

func findbest(scores map[position]map[string]int) int {
    best := 0
    for _, end := range scores {
        for _, score := range end {
            // don't you just love how everything is a for loop with an outside variable?
            if score > best {
                best = score
            }
        }
    }
    return best
}

func solve(valves map[string]valve) int {
    start := makeposition("AA", "AA")
    var minutes [27]map[position]map[string]int
    minutes[0] = make(map[position]map[string]int)
    minutes[0][start] = make(map[string]int)
    minutes[0][start][""] = 0

    var positions []position
    for me := range valves {
        for el := range valves {
            if me > el { continue }
            positions = append(positions, makeposition(me, el))
        }
    }

    for i := 1; i <= 26; i++ {
        timeLeft := 26 - i
        minutes[i] = make(map[position]map[string]int)
        for _, position := range positions {
            minutes[i][position] = make(map[string]int)
            prevpositions := cross(
                append(valves[position.me].tunnels, position.me),
                append(valves[position.el].tunnels, position.el),
            )
            for _, prevposition := range prevpositions {
                for prevstate, score := range minutes[i-1][prevposition] {
                    state := parseState(prevstate)
                    newscore := score
                    // Believe me when I say this condition is the source of all bugs.
                    if (position.me == prevposition.me || position.me == prevposition.el) && valves[position.me].flowrate > 0 && !strings.Contains(state.path(), position.me) {
                        state = state.open(position.me)
                        newscore += timeLeft * valves[position.me].flowrate 
                    }
                    // Actually don't because the pruning was the source of many bugs as well
                    if (position.el == prevposition.el || position.el == prevposition.me) && valves[position.el].flowrate > 0 && !strings.Contains(state.path(), position.el) {
                        state = state.open(position.el)
                        newscore += timeLeft * valves[position.el].flowrate 
                    }
                    if prevscore, has := minutes[i][position][state.path()]; !has || newscore > prevscore {
                        minutes[i][position][state.path()] = newscore
                    }
                }
            }
        }

        // pruning useless states makes this significantly faster in the back half...
        bestSoFar := findbest(minutes[i])
        for position, states := range minutes[i] {
            for statestr, score := range states {
                var unopenedScores []int
                for id, valve := range valves {
                    if valve.flowrate == 0 { continue }
                    if strings.Contains(statestr, id) { continue }
                    unopenedScores = append(unopenedScores, valve.flowrate)
                }
                sort.Ints(unopenedScores)
                potentialScore := score
                for n := 0; n < timeLeft; n++ {
                    if n >= len(unopenedScores) { break }
                    potentialScore += (timeLeft - n) * unopenedScores[len(unopenedScores) - 1 - n]
                }
                if potentialScore < bestSoFar {
                    delete(minutes[i][position], statestr)
                }
            }
        }

        totalStates := 0
        for _, positions := range minutes[i] {
            totalStates += len(positions)
        }
    }

    return findbest(minutes[26])
}


func main() {
    input := bufio.NewReader(os.Stdin)
    valves := make(map[string]valve)
    linere, _ := regexp.Compile(`Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ((?:[A-Z]{2}(?:,\s))*[A-Z]{2})`)
    for {
        line, err := input.ReadString('\n')
        if err != nil { break }
        if line == "" { break }
        matches := linere.FindStringSubmatch(line)
        valveid := matches[1]
        flowrate, _ := strconv.Atoi(matches[2])
        tunnels := strings.Split(matches[3], ", ")
        valves[valveid] = valve { flowrate: flowrate, tunnels: tunnels }
    }
    fmt.Println(solve(valves))
}
