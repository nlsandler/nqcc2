#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""CI script to test the reference implementation for a particular chapter.
Test all combinations of extra credit features and other test options.
This script's main purpose is to test the test suite itself (e.g. make sure
we didn't accidentally use Part II features in what's supposed to be a
Part I-only test)
"""
import argparse
import json
import platform
import subprocess

# usage: /run_tests.py /path/to/cc --chapter 1
# for pre-coalescing chapter 20: ./run_tests.py /path/to/cc --chapter 1 --no-coalesce
parser = argparse.ArgumentParser()
parser.add_argument(
    "cc", type=str, nargs="?", default=None, help="Path to compiler under test"
)
parser.add_argument(
    "--chapter",
    type=str,
    help=("Chapter whose implementation we're testing"),
)

args = parser.parse_args()

CC: str = args.cc
CHAPTER: int
NO_COALESCING: bool = False
if args.chapter == "20a":
    CHAPTER = 20
    NO_COALESCING = True
else:
    CHAPTER = int(args.chapter)

if CHAPTER < 1 or CHAPTER > 20:
    exit("Bad chapter number")

# do we need rosetta?
ARCH_PREFIX = ""
if platform.machine().lower() == "arm64":
    ARCH_PREFIX = "arch -x86_64 "

# figure out what combinations of extra-credit flags to test. Include:
# --extra-credit (all extra credit features that have been implemented so far)
# none (no extra credit features)
# each combo of extra-credit features covered by any test case in this
# chapter or an earlier one
EXTRA_CRED_CHAPTERS = {
    "bitwise": 3,
    "compound": 5,
    "increment": 5,
    "goto": 6,
    "switch": 8,
    "nan": 13,
    "union": 18,
}


def get_idx(flag: str) -> tuple[int, str]:
    """Sort flags by chapter, use alphabetical order as tiebreaker
    Just for consistency/determinism in test runs
    """
    return (EXTRA_CRED_CHAPTERS[flag], flag)


# all extra credit chapters implemented so far
ALL_EXTRA_CREDIT: tuple = tuple(
    sorted(
        [f for f in EXTRA_CRED_CHAPTERS if EXTRA_CRED_CHAPTERS[f] <= CHAPTER],
        key=get_idx,
    )
)

# find set of all extra-credit flag combinations we should test,
# based on test_properties.json, and record appropriate option
# strings to enable each of them (including options passed to test_compiler
# and options passed to the compiler itself
extra_credit_combos: dict[tuple, dict[str, str]] = {
    # none (empty set)
    (): {"test_opts": "", "compiler_opts": ""},
    # all
    ALL_EXTRA_CREDIT: {
        "test_opts": "--extra-credit",
        "compiler_opts": " ".join("--" + f for f in ALL_EXTRA_CREDIT),
    },
}

# if the key-value pair "test/case.c": (flag1, flag2, ...)
# appears in test_properties.json, and test/case.c is in the current
# chapter or earlier, add (flag1, flag2, ...) to our set of extra credit combos
with open("./test_properties.json", encoding="utf-8") as f:
    extra_cred_flags = json.load(f)["extra_credit_tests"]
    for path, flags in extra_cred_flags.items():
        # all keys in this dict start with chapter_N/
        chapter_num = int(path.split("/")[0].removeprefix("chapter_"))
        if chapter_num <= CHAPTER:
            k = tuple(sorted(flags, key=get_idx))
            opt_string = " ".join("--" + flag for flag in k)
            extra_credit_combos[k] = {
                "test_opts": opt_string,
                "compiler_opts": opt_string,
            }


# test functions
def run_tests(test_script_opts: str = "", cc_opts: str = "") -> None:
    """Invoke test_compiler script with a particular set of options.
    Args:
        test_script_opts: options to pass test_compiler
        cc_opts: options to pass the compiler under test (after -- )
    """

    if cc_opts:
        cc_opt_str = f" -- {cc_opts}"
    else:
        cc_opt_str = ""

    cmd = f"{ARCH_PREFIX}./test_compiler {CC} --chapter {CHAPTER} {test_script_opts}{cc_opt_str}"
    print(cmd, flush=True)
    subprocess.run(
        cmd,
        shell=True,
        check=True,
    )


def test_normal() -> None:
    """Main test method for if we're testing a chapter from part I or II"""
    # define the stages we're going to test
    stages: list[str] = ["lex", "parse", "tacky", "validate", "codegen", "run"]
    if CHAPTER == 1:
        # tacky and validate stages not added yet
        stages.remove("tacky")
        stages.remove("validate")
    elif CHAPTER < 5:
        # TACKY stage added, validate not
        stages.remove("validate")
    for _, extra_cred_opts in extra_credit_combos.items():
        test_script_opts = extra_cred_opts["test_opts"]
        compiler_opts = extra_cred_opts["compiler_opts"]
        for stage in stages:
            stage_test_script_opts = f"{test_script_opts} --stage {stage}"
            run_tests(test_script_opts=stage_test_script_opts, cc_opts=compiler_opts)


def test_chapter_19() -> None:
    """Run tests for chapter 19.
    Don't test different stages but do run both with and without
    --int-only option
    """
    for _, extra_cred_opts in extra_credit_combos.items():
        test_script_opts = extra_cred_opts["test_opts"]
        compiler_opts = extra_cred_opts["compiler_opts"]
        # run with --int-only option
        run_tests(
            test_script_opts=test_script_opts + " --int-only",
            cc_opts=compiler_opts + " --int-only",
        )
        # run without it
        run_tests(test_script_opts=test_script_opts, cc_opts=compiler_opts)


def test_chapter_20a() -> None:
    """Run tests for chapter 20 without coalescing.
    Don't test different stages but do run both with and without
    --int-only option.
    """
    for _, extra_cred_opts in extra_credit_combos.items():
        test_script_opts = extra_cred_opts["test_opts"]
        compiler_opts = extra_cred_opts["compiler_opts"]
        # run with --int-only option
        run_tests(
            test_script_opts=test_script_opts + " --int-only --no-coalescing",
            cc_opts=compiler_opts + " --int-only",
        )
        # run without it
        run_tests(
            test_script_opts=test_script_opts + " --no-coalescing",
            cc_opts=compiler_opts,
        )


def test_chapter_20() -> None:
    """Run tests for chapter 20 with coalescing.
    Don't test different stages but do run both with and without
    --int-only option.
    """
    for _, extra_cred_opts in extra_credit_combos.items():
        test_script_opts = extra_cred_opts["test_opts"]
        compiler_opts = extra_cred_opts["compiler_opts"]
        # run with --int-only option
        run_tests(
            test_script_opts=test_script_opts + " --int-only",
            cc_opts=compiler_opts + " --int-only",
        )
        # run without it
        run_tests(test_script_opts=test_script_opts, cc_opts=compiler_opts)


if __name__ == "__main__":
    if CHAPTER < 19:
        test_normal()
    elif CHAPTER == 19:
        test_chapter_19()
    elif CHAPTER == 20:
        if NO_COALESCING:
            test_chapter_20a()
        else:
            test_chapter_20()
    else:
        exit(f"Bad chapter number {CHAPTER}")
    exit(0)
