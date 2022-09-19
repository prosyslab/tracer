# Tracer
[Tracer](https://prosys.kaist.ac.kr/tracer/) is a signature-based static analyzer for detecting recurring vulnerabilities.

## Installation
Run the following command to install Tracer:
```
$ ./build.sh
```
Tracer is implemented on top of [Infer](https://github.com/prosyslab/tracer-infer/tree/master).
The above command will install all the dependencies, Infer, and Tracer.

## Usage

To run Tracer, Python 3.9 or above is required.

You can run Tracer for debian packages or non-debian packages (your local projects).

Both of them generate `tracer-out` with the analysis results of Tracer.

### Debian package

For debian packages, we use the docker environment to build the given package.

Therefore, we assume the `docker` command is available.

```
$ ./bin/tracer --debian --package [PACKAGE_NAME]
```

For example,

```
$ ./bin/tracer --debian --package htmldoc
```


### Non-debian package

For non-debian packages, We assume the root directory of the target package contains a well-defined Makefile for `make` command.

Also, the state of the package root directory should be ready for `make`.

```
$ ./bin/tracer --package [PATH_OF_PACKAGE_ROOTDIR]
```
For example,
```
$ ./bin/tracer --package /home/user/libXcursor-1.1.14 (relative path is also available)
```

## Example

```
$ ./bin/tracer --package test
```

The above command analyzes `test` directory and creates `tracer-out` in the root directory.

In `tracer-out`, there are two report files.

```
$ ls -al tracer-out
total 24
drwxrwxr-x  2 wooseok wooseok  4096 Sep 19 17:37 .
drwxrwxr-x 12 wooseok wooseok  4096 Sep 19 17:37 ..
-rw-rw-r--  1 wooseok wooseok 11219 Sep 19 17:37 test.json
-rw-rw-r--  1 wooseok wooseok    78 Sep 19 17:37 test.txt
```

`txt` file contains summarized results for all reported alarms.

`json` file contains more detailed information like top-scored signature and features.

If you open the `test.txt`, you can see that Tracer finds an integer overflow alarm from `test.c` and scores it.

```
1. src: test.c:21, sink: test.c:11, score: 0.910579, bug_type: "IntOverflow."
```