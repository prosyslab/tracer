# Introduction
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

# Publications

* **Tracer: Signature-based Static Analysis for Detecting Recurring Vulnerabilities**
  Wooseok Kang, Byoungho Son, and Kihong Heo<br>
  CCS 2022: ACM Conference on Computer and Communications Security, 2022

# Artifacts
