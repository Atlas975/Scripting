from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.common.exceptions import NoSuchElementException, WebDriverException
import contextlib


class ScrapeTool:  # use headless=False to show browser
    def __init__(
        self, browser="chrome", start_url="https://www.google.com", headless=True
    ):
        self.path = "/home/adilw/Dropbox/Adil_Code/.web_drivers/"  # path to driver file, webdriver name appended
        self.url = start_url
        match (browser.lower()):
            case "firefox":
                self.path += "geckodriver"
                op = FirefoxOptions()
                op.headless = headless
                try:
                    self.driver = webdriver.Firefox(
                        service=Service(executable_path=self.path), options=op
                    )
                except WebDriverException:
                    print("Firefox driver not found")
            case _:
                self.path += "chromedriver"
                op = ChromeOptions()
                op.headless = headless
                try:
                    self.driver = webdriver.Chrome(
                        service=Service(executable_path=self.path), options=op
                    )
                except WebDriverException:
                    print("Chrome driver not found")

        self.driver.get(self.url)

    def search(self):
        if self.driver:
            try:
                self.driver.get(self.url)
            except Exception:
                print("Invalid URL input")

    def url_append(self, argument, replace_depth=0):
        self.url = (
            "{self.url}/{argument}"
            if replace_depth == 0
            else f"{self.url_remove(replace_depth)}/{argument}"
        )

    def url_remove(self, depth=1):
        for _ in range(depth):
            cutoff = self.url.rfind("/")
            self.url = self.url[:cutoff]

    def element_extract(self, by="xpath", element="//div", type="text", *args):
        focus = self.driver.find_elements(by=by, value=element)
        content = []
        for item in focus:
            for arg in args:
                with contextlib.suppress(NoSuchElementException):
                    match (type):
                        case "text":
                            content.append(f"{item.find_element(by=by,value=arg).text}")
                        case "href":
                            content.append(
                                f"{item.find_element(by=by,value=arg).get_attribute('href')}"
                            )
        return content

    def click(self, by, element, type="class"):
        xml_path = xml_format(by, element, type)
        self.driver.find_element(by="xpath", value=xml_path).click()

    def enter_text(self, **kwargs):
        for text, element in kwargs.items():
            self.driver.find_element(by="xpath", value=element).send_keys(text)

    def wait(self, duration=1):
        self.driver.implicitly_wait(duration)

    def kill_bot(self, display=False):
        self.driver.quit()
        if display:
            print(f"{self} killed")

    @staticmethod
    def xml_format(by, element, type="class", additional=""):
        return f'//{by}[@{type}="{element}"]{additional}'


def xml_format(by, element, type="class", additional=""):
    return f'//{by}[@{type}="{element}"]{additional}'
