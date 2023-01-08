import subprocess as sp
import sys







# print the results of git remote -v
print(sp.run(["git", "remote", "-v"], capture_output=True).stdout.decode())


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        raise ValueError("No command provided")


















if __name__ == '__main__':
    main(sys.argv)
