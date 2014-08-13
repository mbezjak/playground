@Grab(group='net.sourceforge.htmlunit', module='htmlunit', version='2.15')
import com.gargoylesoftware.htmlunit.WebClient

def client = new WebClient()
def page = client.getPage("http://google.com/ncr")

assert page.titleText == 'Google'

page.executeJavaScript("localStorage.a = '1'")
assert page.executeJavaScript("localStorage.a").javaScriptResult as int == 1
