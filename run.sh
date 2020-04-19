nasm -f elf64 Examples/example.asm && ld -o Examples/example.elf Examples/example.o
./Examples/example.elf
echo $?
rm Examples/example.o
rm Examples/example.elf

