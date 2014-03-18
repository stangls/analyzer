
int i=0;

int f(){
  i++;
  return i;
}

void main() {
  i=f(f());
  i++;
  /*
    if the invariant at line 10 (see .ix file) would be required
    after returning from f, we would have deadcode here.
  */
  assert(1); // SUCCESS
}

