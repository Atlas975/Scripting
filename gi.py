import subprocess as sp
import argparse as ap


""" aliases for .config file
alias

"""


class GitParser:
    def __init__(self) -> None:
        self.user = "Atlas975"
        self.remote = f"git@github.com:{self.user}"

        parser = ap.ArgumentParser(prog="gi", usage="gi COMMAND [ARGS ...]")
        parser.add_argument("CMD", help="git command")
        parser.add_argument("ARGS", nargs="*", help="git command arguments")
        self.args = parser.parse_args()

        if not self.args.CMD:
            raise ValueError(self.args.USAGE)





    def alias_match(self, command: str) -> str:
        match command:
            case "ad":
                return "add"
            case "st":
                return "status"
            case "ch":
                return "checkout"
            case "ci":
                return "commit"
            case "br":
                return "branch"
            case "pl":
                return "pull"
            case "ps":
                return "push"
            case "cl":
                return "clone"
            case "in":
                return "init"
            case "rm":
                return "remove"
            case "lo":
                return "log"
            case "me":
                return "merge"
            case "re":
                return "reset"
            case "di":
                return "diff"
            case "
        return command



if __name__ == "__main__":

















if __name__ == '__main__':
    gp = GitParser()
