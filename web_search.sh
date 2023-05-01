#!/bin/bash

BING_SEARCH=0
GOOGLE_SEARCH=0
YOUTUBE_SEARCH=0
WIKIPEDIA_SEARCH=0
WOLFRAM_SEARCH=0
GITHUB_SEARCH=0

while getopts "bgwymc" opt; do
	case $opt in
	b)
		BING_SEARCH=1
		;;
	g)
		GOOGLE_SEARCH=1
		;;
	w)
		WIKIPEDIA_SEARCH=1
		;;
	y)
		YOUTUBE_SEARCH=1
		;;
	m)
		WOLFRAM_SEARCH=1
		;;
	c)
		GITHUB_SEARCH=1
		;;
	\?)
		echo "Invalid option: -$OPTARG" >&2
		;;
	esac
done
shift $((OPTIND - 1))

if [[ $BING_SEARCH -eq 1 ]]; then
	open "https://www.bing.com/search?q=$*"
fi

if [[ $GOOGLE_SEARCH -eq 1 ]]; then
	open "https://www.google.com/search?q=$*"
fi

if [[ $YOUTUBE_SEARCH -eq 1 ]]; then
	open "https://www.youtube.com/results?search_query=$*"
fi

if [[ $WIKIPEDIA_SEARCH -eq 1 ]]; then
	open "https://en.wikipedia.org/wiki/$*"
fi

if [[ $WOLFRAM_SEARCH -eq 1 ]]; then
	open "https://www.wolframalpha.com/input/?i=$*"
fi

if [[ $GITHUB_SEARCH -eq 1 ]]; then
	open "https://github.com/search?q=$*"
fi