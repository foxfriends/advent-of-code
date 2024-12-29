const decoder = new TextDecoder();
let input = "";
for await (const chunk of Deno.stdin.readable) {
  input += decoder.decode(chunk);
}

const castArray = (v) => Array.isArray(v) ? v : [v];

function compare(left, right) {
  if (typeof left === "number" && typeof right === "number") {
    if (left < right) return -1;
    if (left > right) return 1;
    return 0;
  }
  if (Array.isArray(left) && Array.isArray(right)) {
    for (let i = 0; i < left.length; ++i) {
      if (i >= right.length) return 1;
      const child = compare(left[i], right[i]);
      if (child !== 0) return child;
    }
    if (left.length < right.length) return -1;
    return 0;
  }
  return compare(castArray(left), castArray(right));
}

input += "\n[[2]]\n[[6]]";
const packets = input
  .split("\n")
  .filter((x) => !!x)
  .map((str) => JSON.parse(str))
  .sort(compare)
  .map((packet) => JSON.stringify(packet));

const start = packets.indexOf("[[2]]") + 1;
const end = packets.indexOf("[[6]]") + 1;
console.log(start * end);
