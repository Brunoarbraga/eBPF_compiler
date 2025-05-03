eBPF Tools
==========

Framework to facilitate eBPF testing and verification. 
A compiler based on kfl ebpf-tools (https://github.com/kfl/ebpf-tools), with an
added byte-code translation, compilation and execution option.


Build instructions
------------------

Build with:

    $ cabal build

Command-line executable
-----------------------

```
$ cabal exec -- ebpf-tools --help
Usage: ebpf-tools (-c|--compile) INFILE

Available options:
  -c,--compile            Parse eBPF byte-code, represent it as Haskell
                          structures and types, compiles it and executes 
```


eBPF Resources
--------------

See

* <https://ebpf.io>

* [Linux documentation for the eBPF instruction
  set](https://www.kernel.org/doc/Documentation/networking/filter.txt)

* [Instruction set
  reference](https://github.com/iovisor/bpf-docs/blob/master/eBPF.md)

* [ubpf: User-space eBPF VM](https://github.com/iovisor/ubpf/)
