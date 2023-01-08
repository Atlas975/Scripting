from selenium_config import ScrapeTool
from selenium_config import xml_format

stackover_bot = ScrapeTool("firefox", "https://stackoverflow.com/", headless=False)
stackover_bot.search()
input_txt = {"test": xml_format("input", "s-input s-input__search js-search-field ")}
stackover_bot.enter_text(**input_txt)
