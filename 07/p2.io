file  := File clone openForReading("input")
lines := file readLines
file close

VFolder := Object clone
VFolder totalSize := 0
VFolder init := method(
    self files := Map clone
)
VFolder touch := method(path, size,
    self totalSize = self totalSize + size
    (path size > 1) ifTrue (
        self files at(path at(0)) touch(path exSlice(1), size)
    )
)

VFolder mkdirp := method(path,
    if(path size == 1,
        self files atPut(path at(0), VFolder clone),
        self files at(path at(0)) mkdirp(path exSlice(1))
    )
)

VFolder folders := method(
    return(self files values appendSeq(self files values map(folders) flatten))
)


fs := VFolder clone
cwd := list()

lines foreach(line,
    line containsSeq("$ cd ") ifTrue(
        where := line exSlice(5)
        (where == "..") ifTrue(
            cwd pop
            continue
        )
        (where == "/") ifTrue(
            cwd := list()
            continue
        )
        cwd push(where)
        continue
    )
    line containsSeq("dir ") ifTrue(
        dir := cwd clone
        dir push(line exSlice(4))
        fs mkdirp(dir)
        continue
    )
    line containsSeq("$ ls") ifTrue(continue)
    info := line split(" ")
    path := cwd clone
    path push(info at(1))
    fs touch(path, info at(0) asNumber)
)

total := 70000000
required := 30000000
available := total - fs totalSize
toremove := required - available
fs folders select(totalSize >= toremove) map(totalSize) min println
