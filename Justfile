set dotenv-load
set quiet
set shell := ["fish", "-c"]

env_year := env_var_or_default("YEAR", "2024")
session := env_var("SESSION")

relative := trim_start_match(invocation_dir(), justfile_dir())
current_year := replace_regex(relative, "^/(\\d+).*$", "$1")
current_day := replace_regex(relative, "^/\\d+/(\\d+)$|.*", "$1")

default_year := if current_year != "" { current_year } else { env_year }

p1 input="input": (part "p1" current_day current_year input)
p2 input="input": (part "p2" current_day current_year input)
compile part: (_compile current_year current_day part)
run part input="input": (_run current_year current_day part input)

all: (year "2020") (year "2021") (year "2022") (year "2023") (year "2024")
year year=current_year: (day "1" year) (day "2" year) (day "3" year) (day "4" year) (day "5" year) (day "6" year) (day "7" year) (day "8" year) (day "9" year) (day "10" year) (day "11" year) (day "12" year) (day "13" year) (day "14" year) (day "15" year) (day "16" year) (day "17" year) (day "18" year) (day "19" year) (day "20" year) (day "21" year) (day "22" year) (day "23" year) (day "24" year) (day "25" year)
day day=current_day year=current_year: (part "p1" day year) (part "p2" day year)
part part day=current_day year=current_year input="input": (get day year) (_compile year day part) && (_run year day part input)

_compile year day part:
    #!/usr/bin/env fish
    if test ! -d {{year}}/{{day}}
        exit 0
    end
    cd {{year}}/{{day}}
    if test -f {{part}}.erl
        erl -compile {{part}}.erl
    else if test -f {{part}}.swift
        swiftc {{part}}.swift > /dev/null
    else if test -f {{part}}.rs
        rustc {{part}}.rs > /dev/null
    else if test -f {{part}}.c
        gcc {{part}}.c -o {{part}} > /dev/null
    else if test -f {{part}}.cpp
        g++ -std=c++2c {{part}}.cpp -o {{part}} > /dev/null
    else if test -f {{part}}.hs
        ghc {{part}} -O -outputdir.{{part}} > /dev/null
    else if ls | rg "\.cabal\$" -q
        cabal build {{part}} > /dev/null
    else if test -f {{part}}.erl
        erl -compile "$fn"
    else if test -f {{part}}.tri
        trilogy compile {{part}}.tri > {{part}}.ll
        clang-19 -O3 {{part}}.ll -o {{part}}
        rm {{part}}.ll
    else if test -d {{part}} -a -f {{part}}/gleam.toml
        pushd {{part}}
        gleam build
    end

_run year day part input="input":
    #!/usr/bin/env fish
    if test ! -d {{year}}/{{day}}
        exit 0
    end
    cd {{year}}/{{day}}
    if test -x {{part}} -a ! -d {{part}}
        time ./{{part}} < {{input}}
        exit 0
    end
    set fn (fd -d 1 -t f {{part}})
    if test -n "$fn" -a -x "$fn"
        time "./$fn" < {{input}}
    else if test -f {{part}}.pl
        time swipl -s "$fn" -g main,halt < {{input}}
    else if test -f {{part}}.tri
        time trilogy run {{part}}.tri < {{input}}
    else if test -f {{part}}.py
        time python3 "$fn" < {{input}}
    else if test -f {{part}}.rb
        time ruby "$fn" < {{input}}
    else if test -f {{part}}.ts
        time deno "$fn" < {{input}}
    else if test -f {{part}}.php
        time php "$fn" < {{input}}
    else if test -f {{part}}.ex
        time elixir "$fn" < {{input}}
    else if test -d {{part}} -a -f {{part}}/gleam.toml
        pushd {{part}}
        time gleam run "$fn" < ../{{input}}
    else if test -f {{part}}.erl
        time erl -noshell -s {{part}} main -s init stop < {{input}}
    else if ls | rg "\.cabal\$" -q
        time cabal run {{part}} < {{input}}
    end

get day=current_day year=default_year:
    #!/usr/bin/env fish
    if test -f {{year}}/{{day}}/input
        exit 0
    end
    echo "Getting {{year}} day {{day}}"
    mkdir -p {{year}}/{{day}}
    curl https://adventofcode.com/{{year}}/day/{{day}}/input \
        -X GET \
        -H "Cookie: session={{session}}" \
        -H "User-Agent: Cameron Eldridge <cameldridge@gmail.com>" \
        -o {{year}}/{{day}}/input
