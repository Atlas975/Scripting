class ScrapeTool:
    # def __init__(self

    def __init__(self, browser="chrome", headless=True):
        self.path = "/home/adilw/Dropbox/Adil_Code/.web_drivers/"
        self.url = "https://www.google.com"

    @staticmethod
    def xml_format(by, type="class", element="", additional=""):
        return f'//{by}[@{type}="{element}"]{additional}'
