import contextlib
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.firefox.options import Options as FirefoxOptions
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.webdriver.edge.options import Options as EdgeOptions
from selenium.common.exceptions import NoSuchElementException


class ScrapeTool():
    def __init__(self, browser = "chrome", url = "https://www.google.com"):
        self.path = "/home/adilw/Dropbox/Adil_Code/.web_drivers/" # path to web driver file, webdriver name appended
        self.url = url
        self.driver=None
        match(browser.lower()):
            case "chrome":
                self.path += "chromedriver"
                op=ChromeOptions()
                op.headless=True
                try:
                    self.driver = webdriver.Chrome(service = Service(executable_path = self.path),options=op)
                except Exception:
                    print("Chrome driver or browser not found")
            case "edge":
                self.path += "msedgedriver"
                op=EdgeOptions()
                op.headless=True
                try:
                    self.driver = webdriver.Edge(service = Service(executable_path = self.path),options=op)
                except Exception:
                    print("Edge driver or browser not found")
            case "firefox":
                self.path += "geckodriver"
                op = FirefoxOptions()
                op.headless=True
                try:
                    self.driver = webdriver.Firefox(service=Service(executable_path = self.path),options=op)
                except Exception:
                    print("Firefox driver or browser not found")
        self.driver.get(self.url)


    def search(self):
        try:
            self.driver.get(self.url)
        except Exception:
            print("Invalid URL input")

    def url_append(self, argument, replace_depth = 0):
        if replace_depth==0:
            self.url += f"/{argument}"
        else:
            self.url = f"{self.url_remove(replace_depth)}/{argument}"
        # self.url= "{self.url}/{argument}" if replace_depth == 0 else f"{self.url_remove(replace_depth)}/{argument}"

    def url_remove(self, depth = 1):
        for _ in range(depth):
            cutoff = self.url.rfind("/")
            self.url = self.url[:cutoff]
        return self.url

    def element_extract(self, by='xpath', element='//div',type='text',*args):
        focus = self.driver.find_elements(by=by, value=element)
        content=[]
        for item in focus:
            for arg in args:
                with contextlib.suppress(NoSuchElementException):
                    match(type):
                        case 'text':
                            content.append(f"{item.find_element(by=by,value=arg).text}");
                        case 'href':
                            content.append(f"{item.find_element(by=by,value=arg).get_attribute('href')}");
        return content


        # return [item.find_element(by=by, value=f'./{arg}').text for item in focus for arg in args]

    def kill_bot(self,display=False):
        self.driver.quit()
        if display:
            print(f"{self} killed")
