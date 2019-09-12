# bnf 0.0.0
## What is this?
BNF example generator.
### Current lisp world
There is no such library.
### Issues
It is hard to test parser.
### Proposal
BNF provide such feature.
## Usage

```lisp
(bnf:examples (integer (sign? digit+ dot?))
              (sign? (or "" #\+ #\-))
	      (digit+ (or . #.(coerce "1234567890" 'list))
	              :max 3)
              (dot? (or "" #\.)))
=> ("7" "546." "+26" "+794." "-32" "-1.")
```

## From developer

### Product's goal
Already?
### License
MIT
### Developed with
SBCL
### Tested with
* SBCL/1.5.6
* CCL/1.11.5
* ECL/16.1.3

## Installation

