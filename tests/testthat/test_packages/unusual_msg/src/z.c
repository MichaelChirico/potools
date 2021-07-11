#define left1 if (x
#define left2 if (y
#define right && z)

// goal (for #199) is for the file itself to have imbalanced parentheses,
//   but the macro-expanded version is valid
void foo() {
  int a, b;
  int x=1, y=0, z=2;
  left1 right { a = 1; } else { a = 2; }
  left2 right { b = 1; } else { b = 2; }
  error(_("You found me!"));
}
