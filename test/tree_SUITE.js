var fs = require("fs");

V = {};
// we're running this from batiscaph priv dir
eval(fs.readFileSync("wwwroot/app/objtree.js", "utf8"));

process.stdin.resume();
process.stdin.setEncoding('utf8');

var input = "";
process.stdin.on('data', function (chunk) {
  input = input.concat(chunk);
  if (input.slice(-2) == "\n\n") { // end of input
    var delta = JSON.parse(input);
    var layout = {};
    V.updateLayout(delta, layout);
    var tree = V.produceTree(layout);
    process.stdout.write(JSON.stringify(tree));
  }
});
