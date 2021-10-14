# bnf 0.0.0
BNF example generator.

## Altenatives and differences

|          | [cl-string-generator] | [cl-gimei]               | bnf                |
| -------- | --------------------- | ------------------------ | ------------------ |
| generate | any string            | japanese name and adress | any string         |
| via      | regular expression    |                          | bnf                |
| return   | one shot value        | one shot value           | comprehensive list |

[cl-string-generator]: https://github.com/pokepay/cl-string-generator
[cl-gimei]: https://github.com/cxxxr/cl-gimei

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
* CCL/1.12
* ECL/20.4.24
* CLISP/2.49

## Installation

