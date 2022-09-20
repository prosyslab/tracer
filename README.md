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
You can run Tracer for debian packages or your local projects.

### Debian packages

For debian packages, we use the docker environment to build the target package.
Therefore, we assume the `docker` command is available in your environment.

The following command runs Tracer on an official debian package: 
```
$ ./bin/tracer --debian --package [PACKAGE_NAME]
```
where `PACKAGE_NAME` indicates the name of package.


For example, the following command runs Tracer on `htmldoc`:
```
$ ./bin/tracer --debian --package htmldoc
```

### Local projects
For local projects, we assume the root directory of the target project contains a working `Makefile`.

The following command runs Tracer on a given project:
```
$ ./bin/tracer --package [PATH_OF_YOUR_PROJECT]
```

For example, the following command runs Tracer on the [test](test) directory:
```
$ ./bin/tracer --package test
```

## Analysis Results
The analysis results will be stored in the `tracer-out` directory that will be created in the current working directory.
In `tracer-out`, there are two report files:
- `[PACKAGE_NAME].txt`: a summarized report for all alarms.
- `[PACKAGE_NAME].json`: a detailed report for each alarm such as top-ranked signatures, features, and scores.
nformation like top-scored signature and features.
