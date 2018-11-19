path = "https://github.com/daya6489/ShinyApp-/" 
#message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

#chrome.portable = file.path(path,'GoogleChromePortable/App/Chrome-bin/chrome.exe')

# launch.browser = function(appUrl, browser.path=chrome.portable) {
#   message('Browser path: ', browser.path)
#   shell(sprintf('"%s" --app=%s', browser.path, appUrl))
# }
load("Shiny_input.rdata")
#shiny::runApp(file.path(path, "shiny"), launch.browser=launch.browser)
shiny::runApp(launch.browser=T)
# shiny::runApp('./shiny/', launch.browser=T)

runGitHub( "ShinyApp-", "daya6489") 
