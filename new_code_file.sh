#!/bin/bash

add_boilerplate() {
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
	"pl")
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

new_code() {
	local with_boiler=0
	if [[ $1 == "-b" ]]; then
		with_boiler=1
		shift
	fi
	local file="$1"
	if [ -e "$file" ]; then
		printf "\033[95m(%s)\033[92m Opened ${file}\n" "$(uname -or)"

	else
		printf "\033[95m(%s)\033[93m Created ${file}\n" "$(uname -or)"
	fi
	touch "$file"
	code -r "$file"
	if [[ $with_boiler -eq 1 ]]; then
		add_boilerplate "$file"
	fi
}

new_code "$@"


