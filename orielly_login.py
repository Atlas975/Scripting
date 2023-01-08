from selenium_config import ScrapeTool
from selenium_config import xml_format

orielly_bot = ScrapeTool(
    "firefox", "https://www.oreilly.com/library-access/#", headless=False
)
orielly_bot.search()
orielly_bot.click("a", "orm-Link-root listedLink--QAwiL ")
input_txt = {
    "a.wazeer@lancaster.ac.uk": xml_format("input", "orm-Input-input input--f2LI6 ")
}
orielly_bot.enter_text(**input_txt)
orielly_bot.click("button", "orm-Button-root btn--MRgqb ")
orielly_bot.wait(10)
orielly_bot.click("a", "orm-Button-root successBtn--FLaz_  orm-Button-link")
