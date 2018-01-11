library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(fiftystater)

tax09_14 = read.csv("Finalizaed tax09_14.csv", sep=",", header=T, na.strings=c("","."))
tax09_14$AGI_STUB = as.factor(tax09_14$AGI_STUB)
tax09_14$YEAR = as.factor(tax09_14$YEAR)

boolColors = as.character(c("1"="#1f77b4", "2"="#ff7f0e", "3"="#d62728",
                            "4"="#17becf", "5"="#2ca02c", "6"="#9467bd"))
boolScale = scale_colour_manual(name="AGI_STUB", values=boolColors)

ui = fluidPage(
  titlePanel("Standardized Mortgage Interest Paid 2009 to 2014"),
  wellPanel(sliderInput("year", "Choose a Year", 
                        min = 2009, max = 2014,
                        value = 2009, animate = TRUE),
            width = 1),
  tabsetPanel(
    tabPanel("Nationwide Comparison", plotOutput("map")),
    tabPanel("AGI Category Comparison", plotOutput("dot")))
)
  
server = function(input, output) {
  
  data1= reactive({
    df1 = tax09_14[which(tax09_14$YEAR==input$year & tax09_14$AGI_STUB==0), ]
  })
  
  data2 = reactive({
    df2= tax09_14[which(tax09_14$YEAR==input$year & tax09_14$AGI_STUB!=0), ]
  })
  
  output$map = renderPlot({
    ggplot(data=data1(), aes(map_id = STATEABE,fill = A_N19300)) + 
      geom_map(map = fifty_states) + 
      expand_limits(x = fifty_states$long, y = fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) + 
      scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") +
      theme(legend.position = "bottom", 
            panel.background = element_blank()) +
      scale_fill_gradient2(low="#4a8c1c", mid="grey90", high="#bd1100",
                           limits=range(c(statemin,statemax)),
                           midpoint=median(statemedian)) +
      guides(fill=guide_colorbar(title="Standardized Mortgage Interest Paid"))+
      borders("state",colour = "#666666")
  })
  
  output$dot = renderPlot({
    ggplot(data=data2(), aes(x = A_N19300, y = reorder(STATE,A_N19300))) +
      geom_point(size=2, alpha=0.75, aes(colour = AGI_STUB),stat="identity") + 
      scale_x_continuous(limits = c(0, 30),expand = c(0, 0)) + 
      labs(x = "Standardized Mortgage Interest Paid Amount", y="State") +
      theme(axis.text=element_text(size=5)) +
#            axis.title=element_text(size=8,face="bold"))+
      boolScale +
      guides(colour=guide_legend(title="AGI Category"))
  })
}

shinyApp(ui = ui, server = server)
