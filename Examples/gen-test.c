
int x = 3;
int y = 5;

int add(int x, int y) {
  return x + y;
}

int main(void) {
  int max = 1 + 6/3 + 5*2;
  int z = add(add(1, 2), 3);

  while (z < max) {
    z = z + 1;
  }

  return z;
}