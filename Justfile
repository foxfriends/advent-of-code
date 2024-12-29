set dotenv-load
set quiet
set shell := ["fish", "-c"]

default_year := "2024"
session := env_var("SESSION")

[no-cd]
default: p1 p2

[no-cd]
p1 input="input": (compile "p1") && (run "p1" input)

[no-cd]
p2 input="input": (compile "p2") && (run "p2" input)

[no-cd]
compile part:
    #!/usr/bin/env fish
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
    else if test -d {{part}} -a -f {{part}}/gleam.toml
        pushd {{part}}
        gleam build
    end

[no-cd]
run part input="input":
    #!/usr/bin/env fish
    if test -x {{part}}
        time ./{{part}} < {{input}}
        exit 0
    end
    set fn (fd -d 1 -t f {{part}})
    if test -n "$fn" -a -x "$fn"
        time "./$fn" < {{input}}
    else if test -f {{part}}.pl
        time swipl -s "$fn" -g main,halt < {{input}}
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
    else
        echo "Current directory does not contain known solution configuration"
        exit 1
    end

get day year=default_year:
    mkdir -p {{year}}/{{day}}
    curl https://adventofcode.com/{{year}}/day/{{day}}/input \
        -X GET \
        -H "Cookie: session={{session}}" \
        -o {{year}}/{{day}}/input
