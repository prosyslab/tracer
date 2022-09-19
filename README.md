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

For debian package,
```
$ ./bin/tracer --debian --package [PACKAGE_NAME]
```

For non-debian package,
```
$ ./bin/tracer --package [PACKAGE_NAME]
```

We assume the target package contains a well-defined Makefile for `make` and `make clean`.

Both commands generate `tracer-out` with the analysis results of the Tracer.

## Example

```
$ ./bin/tracer --debian --package htmldoc
```

The above command analyzes debian package `htmldoc` and creates `tracer-out` in the root directory.

In `tracer-out`, there are two report files.

```
$ ls -al tracer-out
total 456
drwxrwxr-x  2 wooseok wooseok   4096 Sep 19 12:58 .
drwxrwxr-x 11 wooseok wooseok   4096 Sep 19 12:58 ..
-rw-rw-r--  1 wooseok wooseok 446947 Sep 19 12:58 htmldoc.json
-rw-rw-r--  1 wooseok wooseok   6374 Sep 19 12:58 htmldoc.txt
```

`txt` file contains summarized results for all reported alarms.

`json` file contains more detailed information like top-scored signature and features.