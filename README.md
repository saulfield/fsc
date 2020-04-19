# SimpC

This is a compiler for a subset of C, generating x64 assembly.

Code generation is done naively, stack machine style. However, there are a few small optimizations such as constant-folding.

You can try it out for yourself by running `dotnet run --project Compiler\Compiler.fsproj` from the base repo directory to compile the file `Examples/gen-test.c` and output the file `Examples/example.asm`. Then run `./run.sh` to assemble, link, and run the program, and echo its return code.

# References

- [ISO/IEC 9899:1999 (unofficial version of the C99 Standard)](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf)
- [System V Application Binary Interface AMD64 Architecture Processor Supplement](https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf)
- [Linux System Call Table for x86_64](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/)
- [Douglas Thain, Introduction to Compilers and Language Design](https://www3.nd.edu/~dthain/compilerbook/)
- https://github.com/nlsandler/nqcc