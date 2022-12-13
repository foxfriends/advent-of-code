const decoder = new TextDecoder();
let input = "";
for await (const chunk of Deno.stdin.readable) {
  input += decoder.decode(chunk);
}

const castArray = (v) => Array.isArray(v) ? v : [v];

function compare(left, right) {
  if (typeof left === "number" && typeof right === "number") {
    if (left < right) return true;
    if (left > right) return false;
    return undefined;
  }
  if (Array.isArray(left) && Array.isArray(right)) {
    for (let i = 0; i < left.length; ++i) {
      if (i >= right.length) return false;
      const child = compare(left[i], right[i]);
      if (child !== undefined) return child;
    }
    if (left.length < right.length) return true;
    return undefined;
  }
  return compare(castArray(left), castArray(right));
}

const packets = input.split("\n").filter((x) => !!x).map((str) => JSON.parse(str));
let total = 0;
for (let i = 0; i < packets.length / 2; ++i) {
  const [left, right] = packets.slice(i * 2, i * 2 + 2);
  if (compare(left, right)) total += i + 1;
}

console.log(total);
