class Map
    @@width = 0
    @@height = 0

    def self.width
        @@width
    end

    def self.height
        @@height
    end

    def self.width=(width)
        @@width = width
    end

    def self.height=(height)
        @@height = height
    end
end

class Position
    def initialize(@x : Int32, @y : Int32) end

    def x
        @x
    end

    def y
        @y
    end

    def valid?
        if @x >= 0 && @x < Map.width && @y >= 0 && @y < Map.height
            return true
        end
        return (@x == 0 && @y == -1) || (@x == Map.width - 1 && @y == Map.height)
    end

    def_equals @x, @y
    def_hash @x, @y
end

class Blizzard
    def initialize(@position : Position, @direction : Char) end

    def position
        @position
    end

    def direction
        @direction
    end

    def real?
        return @direction != '.' && @direction != '#'
    end

    def move : Blizzard
        case @direction
        when '>' then return Blizzard.new(Position.new(@position.x + 1, @position.y), @direction)
        when '<' then return Blizzard.new(Position.new(@position.x - 1, @position.y), @direction)
        when '^' then return Blizzard.new(Position.new(@position.x, @position.y - 1), @direction)
        when 'v' then return Blizzard.new(Position.new(@position.x, @position.y + 1), @direction)
        else raise "Invalid direction: #{@direction}"
        end
    end

    def wrap_around : Blizzard
        if @position.x < 0
            return Blizzard.new(Position.new(Map.width - 1, @position.y), @direction)
        elsif @position.x >= Map.width
            return Blizzard.new(Position.new(0, @position.y), @direction)
        elsif @position.y < 0
            return Blizzard.new(Position.new(@position.x, Map.height - 1), @direction)
        elsif @position.y >= Map.height
            return Blizzard.new(Position.new(@position.x, 0), @direction)
        else
            return self
        end
    end

    def_equals @position, @direction
    def_hash @position, @direction
end

init = [] of String
while line = gets
    init.push line
end

Map.width = init[0].size - 2
Map.height = init.size - 2

blizzards = init
    .map_with_index { |line, y|
        line.chars.map_with_index { |char, x| Blizzard.new(Position.new(x - 1, y - 1), char) }
    }
    .flatten
    .select(&.real?)

def solve(start, goal, init)
    cycle = Map.width.lcm(Map.height)
    visited = Set(Tuple(Position, Int32)).new
    queue = [{0, start, init}]

    until queue.empty?
        step, position, blizzard_state = queue.shift

        if position == goal
            return blizzard_state, step
        end

        if visited.includes?({position, step % cycle})
            next
        end

        visited.add({position, step % cycle})

        after_move = blizzard_state
            .map(&.move)
            .map(&.wrap_around)

        blocked_positions = after_move
            .map(&.position)
            .to_set

        options = [
            Position.new(position.x + 1, position.y),
            Position.new(position.x, position.y + 1),
            Position.new(position.x - 1, position.y),
            Position.new(position.x, position.y - 1),
            position,
        ]

        options
            .select(&.valid?)
            .reject { |position| blocked_positions.includes? position }
            .map { |position| {step + 1, position, after_move} }
            .each { |state| queue.push state }
    end

    raise "DIE"
end

start = Position.new(0, -1)
goal = Position.new(Map.width - 1, Map.height)

blizzards, a = solve start, goal, blizzards
blizzards, b = solve goal, start, blizzards
blizzards, c = solve start, goal, blizzards

puts a + b + c
