typedef struct {
  int a;
  int b;
} S;

int add(int x, S s)
{
  return x + s.a + s.b;
}

void main(void)
{
  int x;
  S s;

  x = 1;
  s.a = 10;
  s.b = 12;

  int r = add(x, s);

  return r;
}
