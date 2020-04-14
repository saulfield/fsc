nasm -f elf64 example.asm && ld -o example.elf example.o
./example.elf
echo $?
rm example.o
rm example.elf

