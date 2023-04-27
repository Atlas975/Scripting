import os
import subprocess as sp
import argparse as ap

RD = "\033[91m"

def java_cmd(fpath: str, fname: str, fargs: str, args: ap.Namespace) -> str:
    if args.d:
        return f"cd src && javac -d ../target *.java && cd .. && java -cp ./target {fname} {fargs}"
    return f"javac {fpath} && java {fname} {fargs}"

def rust_cmd(fpath: str, fname: str, fargs: str, args: ap.Namespace) -> str:
    cmd = ""
    if frelative := fpath[:-3]:
        cmd += f"cd {frelative};"
    cmd += f"cargo run"
    if args.o:
        cmd += " --release"
    if args.q:
        cmd += " --quiet"
    return f"{cmd} {fargs}"

def python_cmd(fpath: str, fname: str, fargs: str, args: ap.Namespace) -> str:
    cmd = ""
    if args.fmt:
        cmd += f"black .;"
    cmd += f"python3 {fpath} {fargs}"
    return cmd

def get_cmd(fpath: str, fname: str, fexten: str, args: ap.Namespace) -> str:
    fargs = " ".join(args.FILEARGS)
    match fexten:
        case "py":
            return python_cmd(fpath, fname, fargs, args)
        case "sh":
            return f"{fpath} {fargs}"
        case "rb":
            return f"ruby {fpath} {fargs}"
        case "rs":
            return rust_cmd(fpath, fname, fargs, args)
        case "java":
            return java_cmd(fpath, fname, fargs, args)
        case "js":
            return f"node {fpath} {fargs}"
        case "ts":
            return f"ts-node {fpath} {fargs}"
        case "c":
            return f"gcc {fpath} -o {fname}.o && ./{fname}.o {fargs}"
        case "cpp":
            return f"g++ {fpath} -o {fname}.op && ./{fname}.op {fargs}"
        case "go":
            return f"go run {fpath} {fargs}"
        case "hs":
            return f"runhaskell {fpath} {fargs}"
        case "cs":
            return f"mcs {fpath} && mono {fname}.exe {fargs}"
        case "R":
            return f"Rscript {fpath} {fargs}"
        case "erl":
            return f"erlc {fpath} && erl -noshell -s {fname}:main(). -s init stop"
        case "php":
            return f"php {fpath} {fargs}"
        case "md":
            return f"pandoc {fpath} -o {fname}.pdf"
        case _:
            raise ValueError(f"Unknown extension: {fexten}")


def main(args: ap.Namespace) -> None:
    if not args.FILE:
        raise ValueError(args.USAGE)

    try:
        dirsplit = args.FILE.rfind("/")
        fpath, frun = f"./{args.FILE[:dirsplit + 1]}", args.FILE[dirsplit + 1 :]
        if not os.path.exists(fpath):
            print(f"{RD}{fpath} does not exist, exiting...")
            return
        fname, fexten = frun.split(".", 1)
    except ValueError:
        print(f"{RD}{frun} is not a valid file, exiting...")
        return

    if args.c:
        sp.call(["clear"])
    sp.call(get_cmd(args.FILE, fname, fexten, args), shell=True)

if __name__ == "__main__":
    parser = ap.ArgumentParser(prog="rcd", usage="rcd [-d] FILE [FILEARGS ...]")
    parser.add_argument("-c", action="store_true", help="clear screen before run")
    parser.add_argument("-d", action="store_true", help="compile files from src directory to target")
    parser.add_argument("-o", action="store_true", help="run optimized compilation")
    parser.add_argument("-q", action="store_true", help="run in quiet mode")
    parser.add_argument("--fmt", action="store_true", help="format file before running")
    parser.add_argument("FILE", help="File to run")
    parser.add_argument("FILEARGS", nargs="*", help="Arguments to pass to file")
    main(parser.parse_args())

# ? means 0 or 1
# * means 0 or more
# + means 1 or more
