# Tracer: Signature-based Static Analysis for Detecting Recurring Vulnerabilities

Repository for "Tracer: Signature-based Static Analysis for Detecting Recurring Vulnerabilities",
which appeared in CCS 2022.


## Installation

For installation, you can execute `build.sh`.
```
$ ./build.sh
```
It installs all dependencies and tools needed by the Tracer.


## Usage

To run the Tracer, Python 3.9 or above is required.

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