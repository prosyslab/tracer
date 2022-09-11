#!/usr/bin/env python3

import sys
import json
import os

PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUG_BENCH_DIR = os.path.join(os.path.dirname(PROJECT_ROOT), "bug-bench",
                             "benchmark")

iof_benchmarks = [
    "gimp", "libxcursor", "autotrace", "shntool", "jhead", "sam2p", "optipng"
]


def get_result_dir(bug_type, program):
    return os.path.join(PROJECT_ROOT, "rank-out", bug_type, program)


def get_rank_json(bug_type, sig_program, tar_program):
    result_dir = get_result_dir(bug_type, sig_program)
    return os.path.join(result_dir, "rank-" + tar_program + ".json")


def get_full_bug_type(t):
    if t == "iof":
        return "integer-overflow"
    elif t == "bof":
        return "buffer-overflow"
    elif t == "fs":
        return "format-string"
    else:
        print("Unknown bug type acronym")
        exit(1)


def is_match(rank, file_name, line):
    # TODO: compare filename
    return rank["line_number"] == line


def is_true_bug(bug_typ, rank):
    program = rank["program"]
    bug_type = get_full_bug_type(bug_typ)
    # TODO: get deterministic version from argv
    bench_dir = os.path.join(BUG_BENCH_DIR, program)
    version = os.listdir(bench_dir)[0]
    label_json = os.path.join(bench_dir, version, "label.json")
    with open(label_json) as f:
        labels = json.load(f)
        for label in labels:
            if label["type"] != bug_type:
                continue
            else:
                # TODO: deal with single source
                src_file = label["source"]["file"]
                src_line = label["source"]["line"]
                rank_src = rank["trace"][0]
                if not is_match(rank_src, src_file, src_line):
                    continue
                sink_file = label["sink"]["file"]
                sink_line = label["sink"]["line"]
                rank_sink = rank["trace"][-1]
                if not is_match(rank_sink, sink_file, sink_line):
                    continue
                return True
        return False


def check_found(pair, alarm_lst):
    for a in alarm_lst:
        if pair[0] == a[0] and pair[1] == a[1]:
            return True
    return False


def check_rank(bug_typ, rank, alarm_lst):
    program = rank["program"]
    bug_type = get_full_bug_type(bug_typ)
    # TODO: get deterministic version from argv
    bench_dir = os.path.join(BUG_BENCH_DIR, program)
    version = os.listdir(bench_dir)[0]
    label_json = os.path.join(bench_dir, version, "label.json")
    rank_src = rank["trace"][0]["line_number"]
    rank_sink = rank["trace"][-1]["line_number"]
    rank_pair = [rank_src, rank_sink]
    if check_found(rank_pair, alarm_lst):
        pass
    else:
        with open(label_json) as f:
            labels = json.load(f)
            for label in labels:
                if label["type"] != bug_type:
                    continue
                elif label["source"]["line"] == rank_src and label["sink"][
                        "line"] == rank_sink:
                    rank_pair.append(True)
                    alarm_lst.append(rank_pair)
                    return
            rank_pair.append(False)
            alarm_lst.append(rank_pair)


def get_result(bug_type, sig_program, tar_program, n, threshold, results):
    rank_json = get_rank_json(bug_type, sig_program, tar_program)
    with open(rank_json) as f:
        ranks = json.load(f)
        until_n = min(n, len(ranks))
        i = 0
        true_l = []
        false_l = []
        seen_alarms = []
        for rank in ranks[:until_n]:
            is_interesting = rank["score"] >= threshold
            if is_interesting:
                check_rank(bug_type, rank, seen_alarms)
                if is_true_bug(bug_type, rank):
                    true_l.append(rank)
                else:
                    false_l.append(rank)
            if (not is_interesting) or i == until_n - 1:
                print("==================================")
                print("target program: {}".format(tar_program))
                print("# Total: {}".format(len(ranks)))
                print("# True: {}".format(len(true_l)))
                print("# False: {}".format(len(false_l)))
                # print("last prob: {}".format(rank["score"]))
                break
            i += 1
        j = 1
        result = []
        distribution = ""
        for s in seen_alarms:
            if s[2]:
                distribution = distribution + str(j) + " | "
            j += 1
        print(distribution)
        if distribution:
            result.append(distribution)
        else:
            result.append("-")
        num_true_alarm = 0
        num_false_alarm = 0
        for seen_alarm in seen_alarms:
            if seen_alarm[2]:
                num_true_alarm += 1
            else:
                num_false_alarm += 1
        result.append(str(num_true_alarm))
        result.append(str(len(true_l)))
        result.append(str(num_false_alarm))
        result.append(str(len(false_l)))
        results.append(result)


if __name__ == "__main__":
    bug_type = sys.argv[1]
    signature_program = sys.argv[2]
    top_n = int(sys.argv[3])
    threshold = float(sys.argv[4]) if len(sys.argv) > 4 else 0.
    # TODO: set benchmarks with respect to bug_type
    results = []
    for target_program in iof_benchmarks:
        get_result(bug_type, signature_program, target_program, top_n, threshold, results)
    print("--------- RESULTS ---------")
    for r in results:
        print("\t".join(r))
