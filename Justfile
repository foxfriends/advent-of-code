set dotenv-load
set quiet
set shell := ["fish", "-c"]

[no-cd]
default: p1 p2

all:
    #!/usr/bin/env fish
    for file in (ls)
        if test -d $file
            echo "Day $file"
            just run $file
        end
    end

[no-cd]
p1 input="input": (do "p1" input)

[no-cd]
p2 input="input": (do "p2" input)

[no-cd]
do part input:
    #!/usr/bin/env fish
    if test -f {{part}}.hs
        ghc {{part}} -outputdir.{{part}} > /dev/null
        and time ./{{part}} < {{input}}
    else if test -f {{part}}.fish
        time ./{{part}}.fish < {{input}}
    end

run day:
    cd {{day}} && just

get day:
    mkdir -p {{day}}
    curl https://adventofcode.com/2024/day/{{day}}/input \
        -X GET \
        -H "Cookie: session=$session" \
        -o {{day}}/input
