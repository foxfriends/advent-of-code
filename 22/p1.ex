defmodule P1 do
    def read_map() do
        case IO.gets("") do
            "\n" -> []
            line -> [to_charlist(line) | read_map()]
        end
    end

    def index_of(x, [x | _], n) do n end
    def index_of(x, [_ | xs], n) do index_of(x, xs, n + 1) end
    def index_of(x, xs) do index_of(x, xs, 1) end

    def nth_or(d, _, []) do d end
    def nth_or(d, 0, _) do d end
    def nth_or(_, 1, [x | _]) do x end
    def nth_or(d, n, [_ | xs]) do nth_or(d, n - 1, xs) end

    def at(map, x, y) do
        map
            |> then(&nth_or([], y, &1))
            |> then(&nth_or(?\s, x, &1))
    end

    def wrap_around(map, position) do
        {x, y, _} = next = step(position)
        case at(map, x, y) do
            ?\n -> position
            ?\s -> position
            _ -> wrap_around(map, next)
        end
    end

    def resolve(map, {x, y, r} = position) do
        case at(map, x, y) do
            ?# -> :reject
            ?. -> position
            _ ->
                {x2, y2, _} = wrap_around(map, {x, y, rem(r + 2, 4)})
                case at(map, x2, y2) do
                    ?# -> :reject
                    ?. -> {x2, y2, r}
                end
        end
    end

    def step({x, y, 0}) do {x + 1, y, 0} end
    def step({x, y, 1}) do {x, y + 1, 1} end
    def step({x, y, 2}) do {x - 1, y, 2} end
    def step({x, y, 3}) do {x, y - 1, 3} end

    def is_distance(?R) do false end
    def is_distance(?L) do false end
    def is_distance(?\n) do false end
    def is_distance(_) do true end

    def move(_, position, 0) do position end
    def move(map, position, n) do
        next = step(position)
        case resolve(map, next) do
            :reject -> position
            result -> move(map, result, n - 1)
        end
    end

    def process(_, position, []) do position end
    def process(map, {x, y, r}, [[?L] | directions]) do
        process(map, {x, y, rem(r + 3, 4)}, directions)
    end
    def process(map, {x, y, r}, [[?R] | directions]) do
        process(map, {x, y, rem(r + 1, 4)}, directions)
    end
    def process(map, position, [distance | directions]) do
        distance
            |> to_string()
            |> String.to_integer()
            |> then(&move(map, position, &1))
            |> then(&process(map, &1, directions))
    end

    def main() do
        map = read_map()

        directions = IO.gets("")
            |> String.trim()
            |> to_charlist()
            |> Enum.chunk_by(&is_distance/1)

        start = map
            |> hd()
            |> to_charlist()
            |> then(&index_of(?., &1))

        {x, y, r} = process(map, {start, 1, 0}, directions)
        IO.puts(x * 4 + y * 1000 + r)
    end
end

P1.main()
