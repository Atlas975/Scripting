#!/usr/bin/python3

from itertools import zip_longest
# from scriptlibs.SeleniumConfig import ScrapeTool

from scriptlibs.SelConf import ScrapeTool

BLUE = "\033[94m"
RED = "\033[91m"
GREEN = "\033[92m"
YELLOW = "\033[93m"
CLEAR = "\033[0m"


class BBCscraper(ScrapeTool):
    def __init__(self, browser="chrome", search="https://www.bbc.com//news"):
        super().__init__(browser, search)
        self.focus = '//div[@class="gs-c-promo-body gel-1/2@xs gel-1/1@m gs-u-mt@m"]'
        self.titleLoc = "./div/a/h3"
        self.subtitleLoc = "./div/p"
        self.urlLoc = "./div/a"

    def get_summary(self):
        titles = self.element_extract("xpath", self.focus, "text", self.titleLoc)
        subtitles = self.element_extract("xpath", self.focus, "text", self.subtitleLoc)
        urls = self.element_extract("xpath", self.focus, "href", self.urlLoc)
        return titles, subtitles, urls

    def print_summary(self, priority=False):
        titles, subtitles, hrefs = self.get_summary()
        if priority:
            for title, subtitle, href in zip_longest(titles, subtitles, hrefs, fillvalue=""):
                print(f"{RED}{title}\n{BLUE}{href}\n{CLEAR}{subtitle}\n")
        else:
            for title, subtitle, href in zip_longest(titles, subtitles, hrefs, fillvalue=""):
                print(f"{GREEN}{title}\n{BLUE}{href}\n{CLEAR}{subtitle}\n")

    def __str__(self):
        return "BBCbot"


if __name__ == "__main__":
    bbcbot = BBCscraper("firefox", "https://www.bbc.com/news/")
    print(f"\n{YELLOW}BBC Articles\n")
    bbcbot.print_summary()
    bbcbot.focus = (
        '//div[@class="gs-c-promo-body gs-u-display-none gs-u-display-inline-block@m gs-u-mt@xs gs-u-mt0@m gel-1/3@m"]'
    )
    bbcbot.print_summary(True)
    bbcbot.kill_bot()
