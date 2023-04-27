import sys
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException, WebDriverException
from selenium.webdriver.edge.options import Options as EdgeOptions
from selenium.webdriver.safari.options import Options as SafariOptions
from selenium.webdriver.remote.webdriver import WebDriver


const_test = "test"

class ScrapeTool:
    def __init__(self, browser="msedge", url="www.google.com", headless=False):
        self.url = url
        self.driver = self.get_driver(browser, headless)

    def search(self):
        self.driver.get("https://www.google.com/")

    def url_append(self, argument):
        return f"{self.url}/{argument}"

    def url_remove(self, depth):
        cutoff = len(self.url)
        for _ in range(depth):
            cutoff = self.url.rfind("/", 0, cutoff)
            if cutoff == -1:
                raise ValueError("Removal depth exceeds URL length")

    def get_driver(self, browser, headless) -> WebDriver:
        match browser:
            case "msedge":
                op = EdgeOptions()
                op.add_argument("--headless") if headless else None
                try:
                    return webdriver.Edge(options=op)
                except WebDriverException:
                    print("Edge driver not found")
            case "safari":
                op = SafariOptions()
                op.add_argument("--headless") if headless else None
                try:
                    return webdriver.Safari(options=op)
                except WebDriverException:
                    print("Safari driver not found")
            case _:
                print("Browser not supported")
        sys.exit(1)

    def kill_bot(self, display=False):
        self.driver.quit()


def xml_format(by, element="", additional=""):
    return f'//{by}[@class="{element}"]{additional}'


if __name__ == "__main__":
    st = ScrapeTool()
    st.search()
    # st.kill_bot()