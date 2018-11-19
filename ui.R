library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(ggplot2)
library(devtools)
# install_github("nik01010/dashboardthemes")
#

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Digital Propensity to Respond Models dashboard",titleWidth = 450)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(helpText(strong("Please select options")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-page", icon = icon("send",lib='glyphicon'),
             menuSubItem("Products", href = "https://www.vmware.com/in/products.html"),
             menuSubItem("NSX", href = "https://www.vmware.com/in/products/nsx.html"),
             menuSubItem("vSAN", href = "https://www.vmware.com/in/products/vsan.html"),
             menuSubItem("vSphere", href = "https://www.vmware.com/in/products/vsphere.html"),
             menuSubItem("vRealize", href = "https://www.vmware.com/in/products/vrealize-suite.html"),
             menuSubItem("Horizon", href = "https://www.vmware.com/in/products/horizon.html"),
             menuSubItem("Workstation One", href = "https://www.vmware.com/in/products/workspace-one.html")),
    selectInput(inputId="var_S2",label="Product:",choices=c('NSX','vSAN','vSphere','Horizon','vRealize','WorkspaceOne'),selected = 'NSX' ),
    selectInput(inputId="var_S3",label="Model Quarter:",choices=c('18Q4','19Q1','19Q2','19Q3'),selected = '19Q2' ),
    numericInput("obs1", "Variable Importance: Top", 10),
    #numericInput("obs2", "% Asset captured in the top: ", 10),
    sliderInput("obs", "% Asset captured in the top:", 1,50,value=3),
    checkboxGroupInput("show_vars", "Lift Chart:",width='300px',inline=T,
                       unique(liftDF$model), selected = unique(liftDF$model)),
    radioButtons(inputId="var9",label="Asset",c("Overall"='All',"HoL"="hol","Eval"="eval","PDF"="pdf","Video"="video"),inline = T)
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value0")
  ,valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  )


frow2 <- fluidRow(
                         column( width = 6,
                         box(width = 12,title = "Train data - target distributions",solidHeader = T,status = "primary", collapsible = T,
                             valueBoxOutput("value01",width = 3),
                             valueBoxOutput("value02",width = 3),
                             valueBoxOutput("value03",width = 3),
                             valueBoxOutput("value04",width = 3))),
                      column( width = 6,
                        box(width = 12,title = "Validation data - target distributions",solidHeader = T,status = "primary", collapsible = T,
                            valueBoxOutput("value011",width = 3),
                            valueBoxOutput("value021",width = 3),
                            valueBoxOutput("value031",width = 3),
                            valueBoxOutput("value041",width = 3)))
                         )


frow3 <- fluidRow(
  column(width = 12,
  box(title = "AUC value",width=3,
      solidHeader = F,
    status = "primary"
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px",width = "250px")),
  box(title = "Variable Importance",width = 5,
      solidHeader = F,
      status = "primary"
      ,collapsible = TRUE 
      ,plotOutput("var_imp", height = "300px")),
  box(title = "Asset captured in top lift",width = 4,
      solidHeader = F,
      status = "primary"
      ,collapsible = TRUE ,
      plotOutput("top_lift",height = "300px"))))
  

frow4 <- fluidRow(
  box(
    title = "Cumulative lift chart"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion", height = "300px")
  ), 
  
  box(
    title = "lift table"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    , tableOutput("mytable3")
  )
  

  
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1,frow2,frow3,frow4)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'digital P2R model', header, sidebar, body, skin='red')