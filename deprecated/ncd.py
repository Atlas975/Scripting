#!/usr/bin/python3

import os
import argparse as ap
import subprocess as sp
import platform as pf
from getpass import getuser

EDITOR = "code -r"
OS_VER = f"{pf.system()} {pf.release()}"
OS_USR = getuser()

RD = "\033[91m"
GR = "\033[92m"
PUR = "\033[95m"
BL = "\033[94m"
YE = "\033[93m"
CL = "\033[0m"


def create_file(fpath: str, fname: str, fexten: str) -> None:
    create = f"touch {fpath}"
    access = f"{EDITOR} {fpath}"
    ftype = None

    match fexten:
        case "py":
            ftype = "Python file"
        case "md":
            ftype = "Markdown file"
        case "sh":
            ftype = "Bash file"
        case "rs":
            ftype = "Cargo package"
            fpath = fpath[:-3]
            create = f"cargo new -q {fpath}"
            access = f"{EDITOR} {fpath}/src/main.rs"
        case "java":
            ftype = "Java file"
        case "js":
            ftype = "JavaScript file"
        case "ts":
            ftype = "TypeScript file"
        case "c":
            ftype = "C file"
        case "cpp":
            ftype = "C++ file"
        case "html":
            ftype = "HTML file"
        case "css":
            ftype = "CSS file"
        case "sql":
            ftype = "SQL file"
        case "rb":
            ftype = "Ruby file"
        case "go":
            ftype = "Go file"
        case "hs":
            ftype = "Haskell file"
        case "cs":
            ftype = "C# file"
        case "R":
            ftype = "R file"
        case "erl":
            ftype = "Erlang file"
        case "php":
            ftype = "PHP file"
        case "tex":
            ftype = "LaTeX file"
        case "txt":
            ftype = "Text file"

    if ftype is None:
        print(f"{RD}{fexten}{CL} is an invalid extension, skipping...")
        return

    condition = "created"
    if os.path.exists(fpath):
        condition = f"{PUR}accessed{CL}"
        sp.run(f"{access}", shell=True)
    else:
        sp.run(f"{create} && {access}", shell=True)

    print(
        f"{GR}{ftype}: {BL}{fname}{CL} {condition} by {BL}{OS_USR}{CL} on {YE}{OS_VER}{CL} at {GR}{fpath}{CL}"
    )

def flag_parse(args: ap.Namespace) -> str:
    if args.e:
        return "$DEV"
    if args.o:
        return "$NOTES"
    if args.t:
        return "$DEV/Temp_Code"
    if args.s:
        return "$DEV/Temp_Code/SystemTests"
    return os.getcwd()

def main(args: ap.Namespace) -> None:
    if not args.FILES:
        raise ValueError(args.usage)

    basepath = flag_parse(args)
    sp.run(["clear"])
    for arg in args.FILES:
        try:
            fpath = f'{basepath}/{arg}'
            fname, fexten = fpath[fpath.rfind("/") + 1 :].split(".", 1)
            create_file(fpath, fname, fexten)
        except ValueError:
            print(f"{RD}{arg} is an invalid filename, skipping...{CL}")


if __name__ == "__main__":
    parser = ap.ArgumentParser(prog="ncd", usage="ncd [-e | -o | -t] ... FILES ...")
    parser.add_argument("-e", action="store_true", help="create file in code directory")
    parser.add_argument("-o", action="store_true", help="create file in notes directory")
    parser.add_argument("-t", action="store_true", help="create file in temp directory")
    parser.add_argument("-s", action="store_true", help="create file in system tests directory")
    parser.add_argument("FILES", nargs="+", help="file to create")
    main(parser.parse_args())
