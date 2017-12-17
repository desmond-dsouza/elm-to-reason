Goal: a command-line utility that will slurp in an Elm program (using Elm-Ast) and spit out a Reason program, while trying to minimize any hand-editing required after the translation.

Will probably use:

- For node.js command-line usage https://github.com/lazamar/elm-synchronous
- elm-ast for parsing
- elm-community/graph for topo-sort & SCCs
- fs & fs.readFileSync http://stackabuse.com/read-files-with-node-js/
	 & fs.writeFileSync for file IO from node.js