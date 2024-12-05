set dotenv-load
set quiet
set shell := ["fish", "-c"]

[no-cd]
p1 input="input": (do "p1" input)

[no-cd]
p2 input="input": (do "p2" input)

[no-cd]
do part input:
    #!/usr/bin/env fish
    if test -f {{part}}.hs
        ghc {{part}} -no-keep-hi-files -no-keep-o-files > /dev/null
        time ./{{part}} < {{input}}
    else if test -f {{part}}.fish
        time ./{{part}}.fish < {{input}}
    end

run day part:
    cd {{day}} && just {{part}}

get day:
    mkdir -p {{day}}
    curl https://adventofcode.com/2024/day/{{day}}/input \
        -X GET \
        -H "Cookie: session=$session" \
        -o {{day}}/input
