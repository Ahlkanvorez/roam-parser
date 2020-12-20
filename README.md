# Roam Parser

This is a ClojureScript implementation of a parser for the markup used at [roamresearch.com](https://roamresearch.com), with a live demo using Reagent at [roam-parser.robertm.io](https://roam-parser.robertm.io).

Text with roam markup is parsed into an EDN tree, which can be further traversed and updated using built-in Clojure functions to add dynamic behaviors, such as pattern-based manipulations of text, or evaluation of mathematical expressions.

The parser performs a linear pass over the text, backtracking to generate the parse tree whenever it encounters markup close indicators. This results in the invariant that at each iteration of the loop, all markup whose close character has been encountered has been parsed into a tree successfully.

Backtracking for close tokens also checks that a corresponding open token exists, which is a linear check; this could be optimized so the parser keeps extra state, and knows whether an open term exists without needing to check for one.

Parsed terms become nodes in the tree, and their contents are not traversed again, with the exception of text nodes, which are joined together if adjacent to avoid an excessive number of nodes.

## Development

To run a live demo in a local dev server using shadow-cljs with hot reloads, run:
```
clj -A:dev watch app
```

## Testing

To run the test suites, run:
```
clj -A:dev compile test
```

## Deployment

Since this webapp uses no server-side state or requests, it can be deployed
serverlessly, such as on S3 via CloudFront.

## License

BSD 3-Clause License

Copyright (c) 2020, Robert Mitchell
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.