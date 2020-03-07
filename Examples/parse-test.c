// line comment

/*
  multiline
  comment
*/

int global_x;

int other_func() {
  return 0;
}

int add_one(int x) {
  int result = x + 1;
  return result;
}

int add_ints(int x1, int x2) {
  int result = x1 + x2;
  return result;
}

int main(void) {
  int local_x;
  int x = 1;
  int y = (1 + 1) * 2;
  int z = add_ints(x, y);
  
  if (y > x)
    printf("%d\n", z);

  if (y > x) {
    x = 2;
    printf("%d\n", x);
  }

  if (y > x) {
    z = 4;
  } else {
    z = 5;
  }

  while (z > 0) {
    z = z - 1;
    printf("%d\n", z);
  }

  return 0;
}