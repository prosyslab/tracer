#include <stdio.h>
#include <stdlib.h>

int ToL(char *puffer) {
  return (puffer[0] | puffer[1] << 8 | puffer[2] << 16 | puffer[3] << 24);
}

short ToS(char *puffer) { return (puffer[0] | puffer[1] << 8); }

int ReadImage(int rowbytes) {
  char *buffer = malloc(rowbytes);
  return 0;
}

int ReadBMP(char *name) {
  FILE *fd = fopen(name, "rb");
  if (!fd)
    return 1;

  char buffer[256];
  if (fread(buffer, 256, 1, fd) != 0)
    return -1;

  int w = ToL(&buffer[0]);
  int bc = ToS(&buffer[0]);
  int rowbytes = w * bc + 4;

  char *p = ReadImage(rowbytes);
  return 0;
}
