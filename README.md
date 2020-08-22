# cl-binary-parser

Simple parser from lisp into an octet vector usable for (network) streams.

For an example usage, see ```examples/example-1.lisp```.

It uses ideas and functions from following projects:

- [Practical Common Lisp, Chapter  24. Practical: Parsing Binary Files](http://www.gigamonkeys.com/book/practical-parsing-binary-files.html)
- [frpc](https://github.com/fjames86/frpc)
- [nibbles](https://github.com/froydnj/nibbles)
- [alexandria](https://github.com/keithj/alexandria)
- [flexi-streams](https://github.com/edicl/flexi-streams)

## TODOs
* Test usage of
- [ x ] ```binary-enums```
- [ ~ ] ```floats and doubles```
- [ x ] ```binary-strings```




For a similar project, see [lisp-binary](https://github.com/j3pic/lisp-binary).
