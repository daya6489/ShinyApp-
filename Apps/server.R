
# recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)
# 
# dat_smaple<- readRDS("dat_smaple.rds")
# liftDF <- readRDS("liftDF.rds")
# target_smaple<- readRDS("target_smaple.rds")
# AUC_data<- readRDS("AUC_data.rds")
# var_imp<- readRDS("var_imp.rds")


server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  #load("Shiny_input.rdata")
  # source("dhelp.R")
## UI - Number of Eloqua presents in data set

  data1 <- reactive({
    prodfilt=tolower(input$var_S2)
    dat_smaple$Product <- tolower(dat_smaple$Product)
    cnt <- subset(dat_smaple,Product==prodfilt)
    cnt <- as.data.frame(cnt)
  })
  
    output$value0 <- renderValueBox({
    spps <- data1()
    valueBox(
      formatC(spps$Train, format="d", big.mark=',')
      ,'Train data total eloqua'
      ,icon = icon("users",lib='font-awesome')
      ,color = "yellow")
  })
  
  output$value1 <- renderValueBox({
    spps <- data1()
    valueBox(
      formatC(spps$Valid, format="d", big.mark=',')
      ,"Test data total eloqua"
      ,icon = icon("users",lib='font-awesome')
      ,color = "purple")
  }) 
  
  output$value2 <- renderValueBox({
    spps <- data1()
    valueBox(
      formatC(spps$Score, format="d", big.mark=',')
      ,'Score data total eloqua'
      ,icon = icon("users",lib='font-awesome')
      ,color = "green")
    
  })
  
  ## UI - Number of Target distributions
  
  data11 <- reactive({
    prodfilt=tolower(input$var_S2)
    target_smaple$Product <- tolower(target_smaple$Product)
    cnt <- subset(target_smaple,Product==prodfilt & data=="Train")
    cnt <- as.data.frame(cnt)
  })
  
  data12 <- reactive({
    prodfilt=tolower(input$var_S2)
    target_smaple$Product <- tolower(target_smaple$Product)
    cnt <- subset(target_smaple,Product==prodfilt & data=="Valid")
    cnt <- as.data.frame(cnt)
  })
  
  output$value01 <- renderValueBox({
    spps <- data11()
    valueBox(formatC(spps[spps$Asset %in% "HoL",]$cnt, format="d", big.mark=','), "HoL count", width = 2,color="olive")
      })
    
  output$value02 <- renderValueBox({
    spps <- data11()
    valueBox(formatC(spps[spps$Asset %in% "Eval",]$cnt, format="d", big.mark=','), "Eval count", width = 2,color="maroon")
  })
  
  output$value03 <- renderValueBox({
    spps <- data11()
    valueBox(formatC(spps[spps$Asset %in% "Pdf",]$cnt, format="d", big.mark=','), "Pdf count", width = 2,color="teal")
  })
  
  output$value04 <- renderValueBox({
    spps <- data11()
    valueBox(formatC(spps[spps$Asset %in% "Video",]$cnt, format="d", big.mark=','), "Video count", width =2,color="purple")
  })
  
  output$value011 <- renderValueBox({
    spps <- data12()
    valueBox(formatC(spps[spps$Asset %in% "HoL",]$cnt, format="d", big.mark=','), "HoL count", width = 3,color="olive")
  })
  
  output$value021 <- renderValueBox({
    spps <- data12()
    valueBox(formatC(spps[spps$Asset %in% "Eval",]$cnt, format="d", big.mark=','), "Eval count", width = 3,color="maroon")
  })
  
  output$value031 <- renderValueBox({
    spps <- data12()
    valueBox(formatC(spps[spps$Asset %in% "Pdf",]$cnt, format="d", big.mark=','), "Pdf count", width = 3,color="teal")
  })
  
  output$value041 <- renderValueBox({
    spps <- data12()
    valueBox(formatC(spps[spps$Asset %in% "Video",]$cnt, format="d", big.mark=','), "Video count", width = 3,color="purple")
  })
  
  ## AUC plot
data2 <- reactive({
  assetfilt=input$var9
  # if(assetfilt=="All")
  prodfilt=input$var_S2
  AUC_data$Asset <- tolower(AUC_data$Asset)
  AUC_data$Product <- tolower(AUC_data$Product)
  cnt <- subset(AUC_data,Asset==tolower(assetfilt) & Product==tolower(prodfilt))
  cnt <- as.data.frame(cnt)
  })
  
  output$revenuebyPrd <- renderPlot({
    sapps1 <- data2()
    AUC_o <- sapps1[1,"AUC"]
    AUC_o1 <- 1-AUC_o
    exp.ser <- data.frame(type=c(1,2),value=c(AUC_o,AUC_o1),pos=c(AUC_o,1))
    
    ggplot(exp.ser, aes(x=2, y=value, fill=type))+
      geom_bar(stat="identity",colour="red",fill=c("firebrick2","snow"))+
      xlim(0.5, 2.5) +
      coord_polar(theta = "y")+
      geom_text(x=0.5,y=0,aes(label=AUC_o),size=18,colour="forestgreen")+
      theme_bw()+
      theme(legend.position = "none",
            axis.ticks=element_blank(),axis.text=element_blank(),axis.title=element_blank(),
            panel.grid=element_blank(),panel.border=element_blank())
    
  })
  
  # Variable importance
  data3 <- reactive({
    vifilt=input$obs1
    prodfilt=input$var_S2
    var_imp$Product <- tolower(var_imp$Product)
    cnt <- subset(var_imp,Product==tolower(prodfilt))
    setDT(cnt)
    setorder(cnt,-Gain)
    cnt <- cnt[1:vifilt,]
    cnt <- as.data.frame(cnt)
  })
  
  output$var_imp <- renderPlot({
    varimplot <- data3()
    ggplot(varimplot, aes(y=Gain, x=reorder(Feature,Gain))) +
      geom_bar( stat="identity", position="dodge",fill="tomato")+xlab("")+
      ylab("")+coord_flip()+
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3",linetype = "dashed"), panel.grid.minor = element_blank(),
            panel.border = element_blank(), panel.background = element_blank())
    
  })
  
  # Top lift by Asset
  data4 <- reactive({
    assetfilt=input$var9
        prodfilt=input$var_S2
        lift_table$Product <- tolower(lift_table$Product)
        lift_table$Asset <- tolower(lift_table$Asset)
    if(assetfilt=="All") {
    cnt <- subset(lift_table,Product==tolower(prodfilt))} else
    {cnt <- subset(lift_table,Product==tolower(prodfilt) & Asset ==tolower(assetfilt))}
    cnt <- as.data.frame(cnt)
    
  })
  
  
  output$top_lift <- renderPlot({
    lifttb <- data4()
    lifttb <- lifttb[lifttb$partitions %in% input$obs,c("Asset","Cumprop")]
    ggplot(lifttb, aes(y=Cumprop, x=Asset,fill=Asset,label = paste0(Cumprop))) +
      geom_bar( stat="identity", position="dodge")+xlab("")+
      ylab("")+
      geom_text(size = 5, position=position_dodge(width=0.9),vjust=0)+
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3",linetype = "dashed"), panel.grid.minor = element_blank(),
            panel.border = element_blank(), panel.background = element_blank(),
            legend.position = "none")
  })
  
## Lift table
  output$mytable3 <- renderTable({
    lifttb <- data4()
    setDT(lifttb)
    row_index <- c(1:input$obs)
    lifttb<- lifttb[lifttb$partitions %in% row_index,]
    lifttb <- subset(lifttb,select=c("Product","Asset","Percentile","cnt_elq","cnt","Dprop","Cumprop","Lift"))
    names(lifttb) <- c("PRODUCT","ASSET","PERCENTILE","ELOQUA(n)","TARGET(n)","TARGET(%)","CUM(%)","LIFT")
    (lifttb)
  }) 
  
### Lift chart and Table
  
  data5 <- reactive({
    modelfilt=input$show_vars
    prodfilt =tolower(input$var_S2)
    assetfilt=tolower(input$var9)
    pdata = subset(liftDF, (model %in% modelfilt & Product ==prodfilt & Asset==assetfilt))
    pdata = as.data.frame(pdata)
    })
  
   output$revenuebyRegion <- renderPlot({
    sdsd <- data5()
    ggplot (data = sdsd, aes (x = groups, y= lift, colour = model)) + geom_line () +
      theme (legend.position = "bottom") + scale_x_continuous(breaks = seq (0, 50, by =2)) +
      scale_y_continuous(breaks = seq (0, 100, by =2)) + ylab ("Lift") + 
      ggtitle (paste0("Cum Lift by Two Pctl for product ",input$var_S2," - ",input$var9))+ theme_bw()
    
  })
  
}
