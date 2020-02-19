
void function_1();
int function_2(void);
int add_two_ints(int x1, int x2);

int main(void) {

  // print output using printf, for "print formatted"
  // %d is an integer, \n is a newline
  printf("%d\n", 0); // => Prints 0

  // ints are usually 4 bytes
  int x_int = 0;

  // shorts are usually 2 bytes
  short x_short = 0;

  // chars are guaranteed to be 1 byte
  char x_char = 0;
  // char y_char = 'y'; // Char literals are quoted with ''

  // longs are often 4 to 8 bytes; long longs are guaranteed to be at least
  // 8 bytes
  long x_long = 0;
  long long x_long_long = 0;

  // // floats are usually 32-bit floating point numbers
  // float x_float = 0.0f; // 'f' suffix here denotes floating point literal

  // // doubles are usually 64-bit floating-point numbers
  // double x_double = 0.0; // real numbers without any suffix are doubles

  // sizeof(T) gives you the size of a variable with type T in bytes
  // sizeof(obj) yields the size of the expression (variable, literal, etc.).
  printf("%zu\n", sizeof(int)); // => 4 (on most machines with 4-byte words)

  // If the argument of the `sizeof` operator is an expression, then its argument
  // is not evaluated (except VLAs (see below)).
  // The value it yields in this case is a compile-time constant.
  int a = 1;
  // size_t is an unsigned integer type of at least 2 bytes used to represent
  // the size of an object.
  size_t size = sizeof(a++); // a++ is not evaluated
  printf("sizeof(a++) = %zu where a = %d\n", size, a);
  // prints "sizeof(a++) = 4 where a = 1" (on a 32-bit architecture)

  // Arrays must be initialized with a concrete size.
  char my_char_array[20]; // This array occupies 1 * 20 = 20 bytes
  int my_int_array[20]; // This array occupies 4 * 20 = 80 bytes
  // (assuming 4-byte words)

  // You can initialize an array to 0 thusly:
  char my_array[20] = {0};
  // where the "{0}" part is called an "array initializer".
  // NOTE that you get away without explicitly declaring the size of the array,
  // IF you initialize the array on the same line. So, the following declaration
  // is equivalent:
  char my_array[] = {0};
  // BUT, then you have to evaluate the size of the array at run-time, like this:
  size_t my_array_size = sizeof(my_array) / sizeof(my_array[0]);

  // Comparison operators are probably familiar, but
  // there is no Boolean type in C. We use ints instead.
  // (Or _Bool or bool in C99.)
  // 0 is false, anything else is true. (The comparison
  // operators always yield 0 or 1.)
  3 == 2; // => 0 (false)
  3 != 2; // => 1 (true)
  3 > 2; // => 1
  3 < 2; // => 0
  2 <= 2; // => 1
  2 >= 2; // => 1

  // Logic works on ints
  !3; // => 0 (Logical not)
  !0; // => 1
  1 && 1; // => 1 (Logical and)
  0 && 1; // => 0
  0 || 1; // => 1 (Logical or)
  0 || 0; // => 0

  ///////////////////////////////////////
  // Control Structures
  ///////////////////////////////////////

  if (0) {
    printf("I am never run\n");
  } else if (0) {
    printf("I am also never run\n");
  } else {
    printf("I print\n");
  }

  // While loops exist
  int ii = 0;
  while (ii < 10) { //ANY value less than ten is true.
    printf("%d, ", ii++); // ii++ increments ii AFTER using its current value.
  } // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

  printf("\n");

  // *****NOTES*****:
  // Loops and Functions MUST have a body. If no body is needed:
  int i;
  for (i = 0; i <= 5; i++) {
    ; // use semicolon to act as the body (null statement)
  }
  // Or
  for (i = 0; i <= 5; i++);

  // branching with multiple choices: switch()
  switch (a) {
  case 0: // labels need to be integral *constant* expressions (such as enums)
    printf("Hey, 'a' equals 0!\n");
    break; // if you don't break, control flow falls over labels
  case 1:
    printf("Huh, 'a' equals 1!\n");
    break;
    // Be careful - without a "break", execution continues until the
    // next "break" is reached.
  case 3:
  case 4:
    printf("Look at that.. 'a' is either 3, or 4\n");
    break;
  default:
    // if `some_integral_expression` didn't match any of the labels
    fputs("Error!\n", stderr);
    exit(-1);
    break;
  }

  return 0;
}