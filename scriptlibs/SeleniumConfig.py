from itertools import product
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.webdriver.edge.options import Options as EdgeOptions
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.common.exceptions import NoSuchElementException, WebDriverException
import contextlib


class ScrapeTool:  # use headless=False to show browser
    def __init__(self, browser, start_url, headless=True):
        # path to driver file, webdriver name appended
        driverloc = "/home/adilw/Dropbox/Adil_Code/.web_drivers/"
        self.url = start_url
        driverloc += "geckodriver"
        op = FirefoxOptions()
        op.headless = headless
        try:
            self.driver = webdriver.Firefox(service=Service(executable_path=driverloc), options=op)
        except WebDriverException:
            print("Firefox driver not found")
        # match (browser):
        #     case "firefox":
        #         driverloc += "geckodriver"
        #         op = FirefoxOptions()
        #         op.headless = headless
        #         try:
        #             self.driver = webdriver.Firefox(
        #                 service=Service(executable_path=driverloc), options=op
        #             )
        #         except WebDriverException:
        #             print("Firefox driver not found")
        #     case _:
        #         driverloc += "chromedriver"
        #         op = ChromeOptions()
        #         op.headless = headless
        #         try:
        #             self.driver = webdriver.Chrome(
        #                 service=Service(executable_path=driverloc), options=op
        #             )
        #         except WebDriverException:
        #             print("Chrome driver not found")

        self.driver.get(self.url)

    def search(self):
        self.driver.get("https://www.oreilly.com/library-access/#")

    def url_append(self, argument):
        return f"{self.url}/{argument}"

    def url_remove(self, depth):
        cutoff = len(self.url)
        for _ in range(depth):
            cutoff = self.url.rfind("/", 0, cutoff)
            if cutoff == -1:
                raise ValueError("Removal depth exceeds URL length")
        self.url = self.url[:cutoff]

    def element_extract(self, element="//div", attribute="text", *args):
        focus = self.driver.find_elements(by="xpath", value=element)
        content = []
        for item, arg in product(focus, args):
            with contextlib.suppress(NoSuchElementException):
                content.append(
                    item.find_element(by="xpath", value=arg).text
                    if attribute == "text"
                    else item.find_element(by="xpath", value=arg).get_attribute(attribute)
                )

        return content

    def click(self, by, element, type="class"):
        self.driver.find_element(by="xpath", value=self.xml_format(by, element, type)).click()

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
    def xml_format(by, element, elemtype="class", additional=""):
        return f'//{by}[@{elemtype}="{element}"]{additional}'


# def xml_format(by, element, type="class", additional=""):
#     return f'//{by}[@{type}="{element}"]{additional}'
