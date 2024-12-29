set dotenv-load
set quiet
set shell := ["fish", "-c"]

default_year := "2024"
session := env_var("SESSION")

[no-cd]
default: p1 p2

[no-cd]
p1 input="input": (do "p1" input)

[no-cd]
p2 input="input": (do "p2" input)

[no-cd]
do part input:
    #!/usr/bin/env fish
    set fn (fd -d 1 -t f {{part}})
    if test -n "$fn" -a -x "$fn"
        time ./$fn < {{input}}
    else if test -f {{part}}.pl
        and time swipl -s "$fn" -g main,halt < {{input}}
    else if test -f {{part}}.hs
        ghc {{part}} -O -outputdir.{{part}} > /dev/null
        and time ./{{part}} < {{input}}
    else if ls | rg "\.cabal\$" -q
        cabal build {{part}} > /dev/null
        and time cabal run {{part}} < {{input}}
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
