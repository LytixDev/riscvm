RISC-V 64-bit virtual machine. 

Idea is to use this as the compile-time engine (machine code interpreter) for the metagen compiler instead of using a custom bytecode interpreter. Time will tell if this is a good idea or not.

- Why may this be a bad idea? RV64 is designed to be a good ISA. What metagen needs is a good target for compile-time execution.
- Why may this be a good idea? Feels cool. Makes me excited.

Roadmap:
- Base RV64I
- Integer multiplication and division extension
- Float and/or double extensions

May need some custom extensions to make it easier/more ergonomic to both support the semantics of the metagen language and make it easier to use the vm for compile-time execution



Spec:
- https://www2.eecs.berkeley.edu/Pubs/TechRpts/2016/EECS-2016-118.pdf
