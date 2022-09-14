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

Also, for non-debian package, it has to include `build.sh` which contains build commands for Infer.

```bash
#!/bin/bash

# write your build command here
./configure
$INFER_BIN capture -- make

cp -r infer-out $OUT
```

Both commands generate `tracer-out` that has the summanrized results of the Tracer.
