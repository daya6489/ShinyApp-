path = "https://github.com/daya6489/ShinyApp-/tree/master/Apps/" 
#message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

#chrome.portable = file.path(path,'GoogleChromePortable/App/Chrome-bin/chrome.exe')

# launch.browser = function(appUrl, browser.path=chrome.portable) {
#   message('Browser path: ', browser.path)
#   shell(sprintf('"%s" --app=%s', browser.path, appUrl))
# }
load(file.path("Apps/Shiny_input.rdata"))
#shiny::runApp(file.path(path, "shiny"), launch.browser=launch.browser)
shiny::runApp(file.path("Apps"), launch.browser=T)
# shiny::runApp('./shiny/', launch.browser=T)

