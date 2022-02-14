# bnf 0.0.0
BNF example generator especially for comprehensive test.

## Altenatives and differences

| name                  | generate              | via   | return             |
| --------------------- | --------------------- | ----- | ------------------ |
| [cl-string-generator] | any string            | regex | one value          |
| [cl-gimei]            | japanese name, adress |       | one value          |
| [lorem-ipsum]         | text for place-holder |       | one value          |
| [chancery]            | any string            | rule  | one value          |
| [cl-diceware]         | pass phrase           | dice  | one value          |
| bnf                   | any string            | bnf   | comprehensive list |

If you want bnf parser generator, the projects below may what you want.

* [cl-abnf]: ABNF Parser Generator, per RFC2234.
* [cl-bnf]: A simple BNF parser.

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
* SBCL/2.1.7
* CCL/1.12.1
* CLISP/2.49
* ECL/21.2.1
* Allegro/10.1
* CMUCL/21D
* ABCL/1.8.0

## Installation

<!-- Links -->

[cl-string-generator]: https://github.com/pokepay/cl-string-generator
[cl-gimei]: https://github.com/cxxxr/cl-gimei
[lorem-ipsum]:https://github.com/phoe/lorem-ipsum.git
[chancery]:https://hg.stevelosh.com/chancery
[cl-diceware]:https://github.com/billstclair/cl-diceware.git
[cl-abnf]:https://github.com/dimitri/cl-abnf.git
[cl-bnf]:https://github.com/diasbruno/cl-bnf.git
