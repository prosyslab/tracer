## Introduction
Similar software vulnerabilities recur because developers reuse existing vulnerable code,
or make similar mistakes when implementing the same logic. Recently, various analysis techniques have been proposed
to find _syntactically_ recurring vulnerabilities via code reuse. However, limited attention has been devoted
to _semantically_ recurring ones that share the same vulnerable behavior in different code structures.
We present a general analysis framework, called **Tracer**, for detecting such recurring vulnerabilities.
The main idea is to represent vulnerability signatures as traces over interprocedural data dependencies.
Tracer is based on a taint analysis that can detect various types of vulnerabilities.
For a given set of _known_ vulnerabilities, the taint analysis extracts vulnerable traces and establishes a signature database of them.
When a _new unseen_ program is analyzed, Tracer compares all potentially vulnerable traces reported by the analysis
with the known vulnerability signatures.
Then, Tracer reports a list of potential vulnerabilities ranked by the similarity score.

## Examples
Here is a code snippet describing a security vulnerability found in **gimp-2.6.7** in 2009 (CVE-2009-1570).
```c
gint32 ToL(guchar *puffer) {
  // 2. Transform the given string to an (arbitrarily large) integer
  return (puffer[0] | puffer[1] << 8 | puffer[2] << 16 | puffer[3] << 24);
}
gint16 ToS(guchar *puffer) { return (puffer[0] | puffer[1] << 8); }

gint32 ReadBMP(gchar *name) {
  FILE *fd = fopen(name, "rb");
  if (!fd) return -1;
  // 1. Read a byte string from a file
  if (fread(buffer, Bitmap_File_Head.biSize - 4, fd) != 0)
    return -1;
  Bitmap_Head.biWidth = ToL(& buffer[0x00]);
  Bitmap_Head.biBitCnt = ToS(& buffer[0x0A]);
  // 3. An integer overflow caused by the multiplication
  rowbytes = ((Bitmap_Head.biWidth * Bitmap_Head.biBitCnt - 1) / 32) * 4 + 4;
  image_ID = ReadImage(rowbytes);
  ...
}

gint32 ReadImage(gint rowbytes) {
  /* 4. memory allocation with an overflowed size */
  guchar *buffer = malloc(rowbytes);
  /* 5. uses of variable buffer */
}
```
The vulnerable behavior takes place along with the following execution trace:
1. Read a byte string from a file:<br>
```c
fread(buffer, Bitmap_File_Head.biSize - 4, fd)
```
2. Transform the given string to an (arbitrarily large) integer:<br>
```c
(puffer[0] | puffer[1] << 8 | puffer[2] << 16 | puffer[3] << 24)
```
3. An integer overflow caused by the multiplication:<br>
```c
rowbytes = ((Bitmap_Head.biWidth * Bitmap_Head.biBitCnt - 1) / 32) ...;
```
4. Memory allocation with an overflowed size:
```c
guchar *buffer = malloc(rowbytes);
```
5. Uses of the variable buffer

After 8 years, a similar vulnerability was found in another program, **sam2p-0.49.4** (CVE-2017-16663).
Here is the code snippet:
```c
long ToL(unsigned char *puffer) {
  return (puffer[0] | puffer[1] << 8 | puffer[2] << 16 | puffer[3] << 24);
}
short ToS(unsigned char *puffer) { return (puffer[0] | puffer[1] << 8); }

bitmap_type bmp_load_image(FILE *fd) {
  if (fread(buffer, 18, fd) || (strncmp((const char *)buffer, "BM", 2)))
    FATALP("BMP: not a valid BMP file");
  if (fread(buffer, Bitmap_File_Head.biSize - 4, fd) != 0)
    FATALP("BMP: Error reading BMP file header #3");
  Bitmap_Head.biWidth = ToL(&buffer[0x00]);
  Bitmap_Head.biBitCnt = ToS(&buffer[0x0A]);
  rowbytes = ((Bitmap_Head.biWidth * Bitmap_Head.biBitCnt - 1) / 32) * 4 + 4;
  image.bitmap = ReadImage(rowbytes);
  ...
}

unsigned char *ReadImage(int rowbytes) {
  unsigned char *buffer = (unsigned char *) new char[rowbytes];
}
```
**How can we detect this recurring vulnerability?**
Since they are structurally similar to each other, clone-based approaches might help here.

**Then, how about this one?**
This is yet another vulnerability found in **libXcursor-1.1.14** in 2017 (CVE-2017-16612).
```c
XcursorBool _XcursorReadUInt(XcursorFile *file, XcursorUInt *u) {
  unsigned char bytes[4];
  if ((*file->read)(file, bytes, 4) != 4)
    return XcursorFalse;
  *u = (bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24));
  return XcursorTrue;
}

XcursorImage *_XcursorReadImage(XcursorFile *file) {
  XcursorImage head;
  XcursorImage *image;
  if (!_XcursorReadUInt(file, &head.width)) return NULL;
  if (!_XcursorReadUInt(file, &head.height)) return NULL;
  image = XcursorImageCreate(head.width, head.height);
  ...
}

XcursorImage *XcursorImageCreate(int width, int height) {
  XcursorImage *image;
  /* memory allocation with an overflowed size */
  image = malloc(sizeof(XcursorImage) + width * height * sizeof(XcursorPixel));
  /* initialize struct image */
  return image;
}
```
Although this code snippet looks different from the previous cases, their vulnerable behavirs are semantically the same:
1. Read a byte string from a file:<br>
```c
*file->read(file, bytes, 4)
```
2. Transform the given string to an (arbitrarily large) integer:<br>
```c
(bytes[0] | bytes[1] << 8 | bytes[2] << 16 | bytes[3] << 24)
```
3. An integer overflow caused by the multiplication:<br>
```c
sizeof(XcursorImage) + width * height * sizeof(XcursorPixel)
```
4. Memory allocation with an overflowed size:
```c
image = malloc(sizeof(XcursorImage) + width * height * sizeof(XcursorPixel))
```
5. Uses of the variable image

**How can we detect this recurring vulnerability?**

For a given set of _known_ vulnerabilities, Tracer extracts vulnerable traces and establishes a signature database of them.
When a _new unseen_ program is analyzed, Tracer compares all potentially vulnerable traces reported by the analysis
with the known vulnerability signatures.
Then, Tracer reports a list of potential vulnerabilities ranked by the similarity score.
In this case, given the first vulnerbility (CVE-2019-1570), Tracer precisely detects the last one (CVE-2017-16612)
with a high similarity score, 0.96.

## Publications

* **Tracer: Signature-based Static Analysis for Detecting Recurring Vulnerabilities**
  [Wooseok Kang](https://kangwoosukeq.github.io), Byoungho Son, and [Kihong Heo](https://kihongheo.kaist.ac.kr)<br>
  CCS 2022: ACM Conference on Computer and Communications Security, 2022

## Artifacts

We provide the artifacts image, which contains datasets and programs used by evaluating Tracer.
You can access the artifacts image via DockerHub at this [link](https://hub.docker.com/repository/docker/prosyslab/tracer-artifacts).
