import gleam/bool
import gleam/dict
import gleam/erlang
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string

fn count_digits(n: Int) {
  case n {
    n if n < 10 -> 1
    n -> 1 + count_digits(n / 10)
  }
}

fn pow(n, i) {
  case i {
    0 -> 1
    1 -> n
    i -> n * pow(n, i - 1)
  }
}

fn update_stone(stone) {
  case stone {
    0 -> [1]
    n -> {
      let len = count_digits(n)
      case len % 2 {
        0 -> [n / pow(10, len / 2), n % pow(10, len / 2)]
        1 -> [n * 2024]
        _ -> panic
      }
    }
  }
}

fn count(items) {
  use d, #(item, n) <- list.fold(items, dict.new())
  use prev <- dict.upsert(d, item)
  option.unwrap(prev, 0) + n
}

fn spread_second(p) {
  let #(f, s) = p
  list.map(f, pair.new(_, s))
}

fn blink(stones, n) {
  use <- bool.guard(n == 0, stones)
  stones
  |> dict.to_list()
  |> list.map(pair.map_first(_, update_stone))
  |> list.flat_map(spread_second)
  |> count()
  |> blink(n - 1)
}

fn show(stones) {
  stones |> dict.values() |> int.sum() |> io.debug()
}

pub fn main() {
  erlang.get_line("")
  |> result.unwrap("")
  |> string.trim()
  |> string.split(" ")
  |> list.try_map(int.parse(_))
  |> result.unwrap([])
  |> list.map(pair.new(_, 1))
  |> count()
  |> blink(25)
  |> function.tap(show)
  |> blink(50)
  |> show
}
