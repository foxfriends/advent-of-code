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
    id string
    flowrate int
    tunnels []string
}

func dijkstra(valves []valve, previous string) map[string]int {
    dist := make(map[string]int)
    var queue []valve
    for _, v := range valves {
        dist[v.id] = int(^uint(0) >> 1)
        queue = append(queue, v)
    }
    dist[previous] = 0

    for len(queue) > 0 {
        minIndex := 0
        for i, v := range queue {
            if dist[v.id] < dist[queue[minIndex].id] { minIndex = i }
        }
        current := queue[minIndex]
        queue[minIndex] = queue[len(queue) - 1]
        queue = queue[:len(queue) - 1]

        for _, neighbour := range current.tunnels {
            distance := dist[current.id] + 1
            if distance < dist[neighbour] {
                dist[neighbour] = distance
            }
        }
    }

    return dist
}

func condense(valves []valve, valverates map[string]int) map[string]map[string]int {
    graph := make(map[string]map[string]int)
    for current, _ := range valverates {
        graph[current] = make(map[string]int)
        for target, distance := range dijkstra(valves, current) {
            if _, hasrate := valverates[target]; hasrate {
                graph[current][target] = distance + 1 // +1 for the time to open the valve
            }
        }
    }

    return graph
}

func addToPath(pathStr, valve string) string {
    path := append(strings.Split(pathStr, ","), valve)
    sort.Strings(path)
    return strings.Join(path, ",")
}

func solve(graph map[string]map[string]int, valverates map[string]int) int {
    var minutes [31]map[string]map[string]int // what a line, eh?
    minutes[0] = make(map[string]map[string]int)
    minutes[0]["AA"] = make(map[string]int)
    minutes[0]["AA"]["AA"] = 0
    for i := 1; i <= 30; i++ {
        timeLeft := 30 - i
        minutes[i] = make(map[string]map[string]int)
        for valve, rate := range valverates {
            minutes[i][valve] = make(map[string]int)
            for previous, distance := range graph[valve] {
                if i < distance { continue }
                pathsToPrevious := minutes[i - distance][previous]
                for path, score := range pathsToPrevious {
                    if strings.Contains(path, valve) {
                        minutes[i][valve][path] = score
                    } else {
                        newpath := addToPath(path, valve)
                        newscore := score + timeLeft * rate
                        if newscore > minutes[i][valve][newpath] {
                            minutes[i][valve][newpath] = newscore
                        }
                    }
                }
            }
        }
    }
    best := 0
    for _, end := range minutes[30] {
        for _, score := range end {
            // don't you just love how everything is a for loop with an outside variable?
            if score > best {
                best = score
            }
        }
    }
    return best
}


func main() {
    input := bufio.NewReader(os.Stdin)
    var valves []valve
    linere, _ := regexp.Compile(`Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ((?:[A-Z]{2}(?:,\s))*[A-Z]{2})`)
    for {
        line, err := input.ReadString('\n')
        if err != nil { break }
        if line == "" { break }
        matches := linere.FindStringSubmatch(line)
        valveid := matches[1]
        flowrate, _ := strconv.Atoi(matches[2])
        tunnels := strings.Split(matches[3], ", ")
        valves = append(valves, valve { id: valveid, flowrate: flowrate, tunnels: tunnels })
    }

    // find only the relevant valves
    valverates := make(map[string]int)
    valverates["AA"] = 0
    for _, valve := range valves {
        if valve.flowrate != 0 {
            valverates[valve.id] = valve.flowrate
        }
    }

    // simplify the problem. is that necessary? who knows. sure seems useful
    condensed := condense(valves, valverates)
    fmt.Println(solve(condensed, valverates))
}
