from selenium_config import ScrapeTool

orielly_bot = ScrapeTool("firefox","https://www.oreilly.com/library-access/#",headless=False)
orielly_bot.search()
orielly_bot.click('a','orm-Link-root listedLink--QAwiL ')
details={'a.wazeer@lancaster.ac.uk':orielly_bot.xml_format('input','orm-Input-input input--f2LI6 ')}
orielly_bot.enter_text(**details)
orielly_bot.click('button','orm-Button-root btn--MRgqb ')
orielly_bot.driver.implicitly_wait(10)
orielly_bot.click('a','orm-Button-root successBtn--FLaz_  orm-Button-link')
