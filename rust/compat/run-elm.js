#!/usr/bin/env node
// Feed a JSON batch on stdin to the compiled Harness.elm and print its answer.
// Usage: node run-elm.js path/to/harness.js < batch.json > results.json
const path = require("path");
const { Elm } = require(path.resolve(process.argv[2]));

let input = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => (input += chunk));
process.stdin.on("end", () => {
  const app = Elm.Harness.init({ flags: JSON.parse(input) });
  app.ports.done.subscribe((out) => {
    process.stdout.write(JSON.stringify(out));
  });
});
