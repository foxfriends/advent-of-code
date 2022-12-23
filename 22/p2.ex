defmodule P1 do
    def read_map() do
        case IO.gets("") do
            "\n" -> []
            line -> [to_charlist(line) | read_map()]
        end
    end

    def to_face([[?\s | _] | _]) do :empty end
    def to_face(face) do face end

    def make_cube(map) do
        map
            |> Enum.map(&Enum.chunk_every(&1, 50, 50, :discard))
            |> Enum.chunk_every(50, 50, :discard)
            |> Enum.map(fn faces -> Enum.zip_with(faces, & &1) end)
            |> Enum.map(fn faces -> Enum.map(faces, &to_face/1) end)
    end

    def index_of(x, [x | _], n) do n end
    def index_of(x, [_ | xs], n) do index_of(x, xs, n + 1) end
    def index_of(x, xs) do index_of(x, xs, 0) end

    def nth_or(d, _, :empty) do d end
    def nth_or(d, _, []) do d end
    def nth_or(_, 0, [x | _]) do x end
    def nth_or(d, n, [_ | xs]) do nth_or(d, n - 1, xs) end

    def at(cube, x, y) do
        cube
            |> then(&nth_or([], div(y, 50), &1))
            |> then(&nth_or([], div(x, 50), &1))
            |> then(&nth_or([], rem(y, 50), &1))
            |> then(&nth_or(?\s, rem(x, 50), &1))
    end

    # I'm sure there's a real way to do this... but I will just hardcode
    # it to my cube for today.
    def next_face(1, 0, 0) do {2, 0, 0} end
    def next_face(1, 0, 1) do {1, 1, 1} end
    def next_face(1, 0, 2) do {0, 2, 0} end
    def next_face(1, 0, 3) do {0, 3, 0} end

    def next_face(2, 0, 0) do {1, 2, 2} end
    def next_face(2, 0, 1) do {1, 1, 2} end
    def next_face(2, 0, 2) do {1, 0, 2} end
    def next_face(2, 0, 3) do {0, 3, 3} end

    def next_face(1, 1, 0) do {2, 0, 3} end
    def next_face(1, 1, 1) do {1, 2, 1} end
    def next_face(1, 1, 2) do {0, 2, 1} end
    def next_face(1, 1, 3) do {1, 0, 3} end

    def next_face(0, 2, 0) do {1, 2, 0} end
    def next_face(0, 2, 1) do {0, 3, 1} end
    def next_face(0, 2, 2) do {1, 0, 0} end
    def next_face(0, 2, 3) do {1, 1, 0} end

    def next_face(1, 2, 0) do {2, 0, 2} end
    def next_face(1, 2, 1) do {0, 3, 2} end
    def next_face(1, 2, 2) do {0, 2, 2} end
    def next_face(1, 2, 3) do {1, 1, 3} end

    def next_face(0, 3, 0) do {1, 2, 3} end
    def next_face(0, 3, 1) do {2, 0, 1} end
    def next_face(0, 3, 2) do {1, 0, 1} end
    def next_face(0, 3, 3) do {0, 2, 3} end

    # Probably a smarter way to do this too... :shrug:
    # Straight
    def rotate_coord(x, y, 0, 0) do { 49 - x, y } end
    def rotate_coord(x, y, 2, 2) do { 49 - x, y } end
    def rotate_coord(x, y, 1, 1) do { x, 49 - y } end
    def rotate_coord(x, y, 3, 3) do { x, 49 - y } end

    # 90 CW
    def rotate_coord(_, y, 0, 1) do { 49 - y, 0 } end
    def rotate_coord(x, _, 1, 2) do { 49, x } end
    def rotate_coord(_, y, 2, 3) do { 49 - y, 49 } end
    def rotate_coord(x, _, 3, 0) do { 0, x } end

    # 90 CCW
    def rotate_coord(_, y, 0, 3) do { y, 49 } end
    def rotate_coord(x, _, 3, 2) do { 49, 49 - x } end
    def rotate_coord(_, y, 2, 1) do { y, 0 } end
    def rotate_coord(x, _, 1, 0) do { 0, 49 - x } end

    # Flip 180
    def rotate_coord(x, y, 0, 2) do { x, 49 - y } end
    def rotate_coord(x, y, 2, 0) do { x, 49 - y } end
    def rotate_coord(x, y, 1, 3) do { 49 - x, y } end
    def rotate_coord(x, y, 3, 1) do { 49 - x, y } end

    def wrap_face({x, y, r}) do
        face_y = div(y, 50)
        face_x = div(x, 50)
        cell_y = rem(y, 50)
        cell_x = rem(x, 50)

        {face_x2, face_y2, r2} = next_face(face_x, face_y, r)
        {cell_x2, cell_y2} = rotate_coord(cell_x, cell_y, r, r2)
        {face_x2 * 50 + cell_x2, face_y2 * 50 + cell_y2, r2}
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
    def move(cube, position, n) do
        {nx, ny, _} = next = step(position)
        resolved = case at(cube, nx, ny) do
            ?# -> :reject
            ?. -> next
            _ ->
                {x2, y2, _} = wrapped = wrap_face(position)
                case at(cube, x2, y2) do
                    ?# -> :reject
                    ?. -> wrapped
                end
        end

        case resolved do
            :reject -> position
            _ -> move(cube, resolved, n - 1)
        end
    end

    def process(_, position, []) do position end
    def process(cube, {x, y, r}, [[?L] | directions]) do
        process(cube, {x, y, rem(r + 3, 4)}, directions)
    end
    def process(cube, {x, y, r}, [[?R] | directions]) do
        process(cube, {x, y, rem(r + 1, 4)}, directions)
    end
    def process(cube, position, [distance | directions]) do
        distance
            |> to_string()
            |> String.to_integer()
            |> then(&move(cube, position, &1))
            |> then(&process(cube, &1, directions))
    end

    def main() do
        map = read_map()

        cube = make_cube(map)

        directions = IO.gets("")
            |> String.trim()
            |> to_charlist()
            |> Enum.chunk_by(&is_distance/1)

        start = map
            |> hd()
            |> to_charlist()
            |> then(&index_of(?., &1))

        {x, y, r} = process(cube, {start, 0, 0}, directions)
        IO.puts((x + 1) * 4 + (y + 1) * 1000 + r)
    end
end

P1.main()
