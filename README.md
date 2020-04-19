# SimpC

This is a compiler for a subset of C, generating x64 assembly.

# Limitations

- Only `int` type allowed
- No preprocessor

# TODO

- if-statement codegen
- while loop codegen
- constant folding

# References

- [ISO/IEC 9899:1999 (unofficial version of the C99 Standard)](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf)
- [System V Application Binary Interface AMD64 Architecture Processor Supplement](https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-1.0.pdf)
- [Linux System Call Table for x86_64](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/)
- [Douglas Thain, Introduction to Compilers and Language Design](https://www3.nd.edu/~dthain/compilerbook/)
- https://github.com/nlsandler/nqcc