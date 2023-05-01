#!/bin/bash

EDITOR="code -r"
WITH_BOILER=0
DISABLE_OPEN=0
DISABLE_DIR=0
VERBOSE=0
GREEN="\033[92m"
YELLOW="\033[93m"
RED="\033[91m"




boilerplate_option() {
	if [[ $WITH_BOILER -ne 1 ]]; then # if boilerplate is not enabled, return
		return
	fi
	local fullFileName="$1"
	local dir="$(dirname "$fullFileName")"
	local dirWithoutTrailingSlash="${dir%/}"
	local fileName="$(basename "$fullFileName")"
	local fileNameWithoutExt="${fileName%.*}"
	local ext="${fileName##*.}"
	local boil=""
	local ind="    "
	
	case $ext in
	"py")
		boil="\nif __name__ == \"__main__\":\n${ind}..."
		;;
	"sh")
		boil="#!/bin/bash\n\n"
		;;
	"java")
		boil="public class $fileNameWithoutExt{\n${ind}\n}"
		;;
	"c")
		boil="#include <stdio.h>\n\nint main() {\n${ind}return 0;\n}"
		;;
	"cpp")
		boil="#include <iostream>\n\nusing namespace std;\n\nint main() {\n${ind}return 0;\n}"
		;;
	"rs")
		boil="fn main(){\n${ind}\n}"
		;;
	"html")
		boil="<!DOCTYPE html>\n<html>\n${ind}<head>\n${ind}\t<title>${fileNameWithoutExt}</title>\n${ind}</head>\n${ind}<body>\n${ind}\n</body>\n</html>"
		;;
	"css")
		boil="body {\n${ind}\n}"
		;;
	"md")
		boil="# $fileNameWithoutExt"
		;;
	"tex")
		boil="\\documentclass{article}\n\n\\begin{document}\n\n\\end{document}"
		;;
	"json")
		boil="{\n${ind}\n}"
		;;
	"php")
		boil="<?php\n\n?>"
		;;
	"dart")
		boil="void main() {\n${ind}\n}"
		;;
	"groovy")
		boil="class ${fileNameWithoutExt} {\n${ind}\n}"
		;;
	"hs")
		boil="main :: IO ()\nmain = do\n${ind}\n"
		;;
	"scala")
		boil="object ${fileNameWithoutExt} {\n${ind}def main(args: Array[String]): Unit = {\n${ind}\n${ind}}\n}"
		;;
	"kt")
		boil="fun main(args: Array<String>) {\n${ind}\n}"
		;;
	"js")
		boil="console.log(\"Hello World!\");"
		;;
	"rb")
		boil="puts \"Hello World!\""
		;;
	"swift")
		boil="print(\"Hello World!\")"
		;;
	"pu")
		boil="print \"Hello World!\""
		;;
	"lua")
		boil="print(\"Hello World!\")"
		;;
	"r")
		boil="print(\"Hello World!\")"
		;;
	"erl")
		boil="-module(${fileNameWithoutExt}).\n-export([main/0]).\n\nmain() ->\n${ind}io:fwrite(\"Hello, world!\").\n"
		;;
	"cs")
		boil="using System;\n\nnamespace ${fileNameWithoutExt} {\n${ind}class Program {\n${ind}\tstatic void Main(string[] args) {\n${ind}\t\tConsole.WriteLine(\"Hello World!\");\n${ind}\t}\n${ind}}\n}"
		;;
	"gitignore")
		boil="*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea\n*.class\n*.jar\n*.war\n*.ear\n*.iml\n*.ipr\n*.iws\n*.DS_Store\n*.log\n*.log.*\n*.out\n*.iml\n*.idea"
		;;
	esac
	printf '%b' "$boil" >>"$1"
}
open_option() {
	if [[ $DISABLE_OPEN -eq 1 ]]; then # if opening is disabled, return
		return
	fi
	$EDITOR "$1"
}

directory_option() {
	if [[ -d "$1" ]]; then # if directory exists, return
		return
	fi
	if [[ $DISABLE_DIR -eq 1 ]]; then # if directory creation is disabled and directory doesn't exist, exit
		printf "${RED}Error: Directory %s not found, -d prevents directory creation, exiting...\n" "$1"
		exit 1
	fi
	mkdir -p "$1"

	if [[ $VERBOSE -eq 1 ]]; then
		printf "${YELLOW}Created directory %s\n" "$1"
	fi
}

create_file() {
	if [[ ! -f "$1" ]]; then # if file doesn't exist, create it
		dir=$(dirname "$1")
		file=$(basename "$1")
		directory_option "$dir"
		touch "$1"

		if [[ $VERBOSE -eq 1 ]]; then
			printf "${YELLOW}Created file %s\n" "$file"
		fi
	fi

	if [[ $VERBOSE -eq 1 ]]; then
		printf "${GREEN}Opened %s\n" "$1"
	fi
	
	boilerplate_option "$1"
	open_option "$1"
}

while getopts 'bdov' opt; do # parse options
	case "$opt" in
	b)
		WITH_BOILER=1
		;;
	d)
		DISABLE_DIR=1
		;;
	o)
		DISABLE_OPEN=1
		;;
	v)
		VERBOSE=1
		;;
	\?)
		printf "${RED}Usage: ncd [-b] [-d] file ...\n" >&2
		exit 1
		;;
	esac
done
shift $((OPTIND - 1)) # shift options so that only file arguments remain

for file in "$@"; do # loop through file arguments
	create_file "$file"
done
