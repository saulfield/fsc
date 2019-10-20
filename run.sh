dotnet run
nasm -f elf64 test1.asm && ld -o test1.elf test1.o
./test1.elf
echo $?