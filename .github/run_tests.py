#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import json
import platform
import subprocess
import sys

# get/sanity check chapter number
cc: str = sys.argv[1]
chapter: int = int(sys.argv[2])
if chapter < 1 or chapter > 20:
    exit("Bad chapter number")

# do we need rosetta?
prefix = ""
if platform.machine().lower() == "arm64":
    prefix = "arch -x86_64 "

# define the stages we're going to test
stages: list[str] = ["lex", "parse", "tacky", "validate", "codegen", "run"]
if chapter == 1:
    # tacky and validate stages not added yet
    stages.remove("tacky")
    stages.remove("validate")
elif chapter < 5:
    # TACKY stage added, validate not
    stages.remove("validate")
elif chapter > 18:
    # don't test intermediate stages for chapters 19/20
    stages = ["run"]

# info about extra credit features
ALL_EXTRA_CRED = {
    "bitwise": 3,
    "compound": 5,
    "increment": 5,
    "goto": 6,
    "switch": 8,
    "nan": 13,
    "union": 18,
}


def get_idx(flag: str) -> tuple[int, str]:
    # sort flags by chapter, use alphabetical order as tiebreaker
    return (ALL_EXTRA_CRED[flag], flag)


all_implemented_extra_credit: tuple = tuple(
    sorted([f for f in ALL_EXTRA_CRED if ALL_EXTRA_CRED[f] <= chapter], key=get_idx)
)


# find set of all extra-credit flag combinations we should test
extra_credit_combos: set[tuple] = {
    # none (empty set)
    (),
    # all
    all_implemented_extra_credit,
}

with open("./test_properties.json", encoding="utf-8") as f:
    extra_cred_flags = json.load(f)["extra_credit_tests"]
    for path, flags in extra_cred_flags.items():
        # all keys in this dict start with chapter_N/
        chapter_num = int(path.split("/")[0].removeprefix("chapter_"))
        if chapter_num <= chapter:
            extra_credit_combos.add(tuple(sorted(flags, key=get_idx)))


for flags in sorted(extra_credit_combos):
    extra_cred_args: str = ""
    if flags:
        flag_str = " ".join("--" + f for f in flags)
        test_compiler_arg = (
            "--extra-credit" if flags == all_implemented_extra_credit else flag_str
        )
        extra_cred_args = f"{test_compiler_arg} -- {flag_str}"
        print(f"Testing with extra credit features: {flag_str}")
    for stage in stages:
        print(f"Testing '{stage}' stage")
        cmd = f"{prefix}./test_compiler {cc} --chapter {chapter} --stage {stage} {extra_cred_args}"
        print(cmd)
        subprocess.run(
            cmd,
            shell=True,
            check=True,
        )
