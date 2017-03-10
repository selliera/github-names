# github-names

A Clojure application designed to obtain some statistics on project names as
published on github

## Usage

You can analyse a file containging names (one per line) using:

    lein run -i <file>

You can fetch project names from github using

    lein run -s <start> -n <count>

where `start` gives the starting point in the github projects listing
(optional), and `count` configures how many names you want.

You can use the `-f` option to save the output to a file.

## License

Copyright 2017 Antoine Reilles

Distributed under the Eclipse Public License either version 1.0
