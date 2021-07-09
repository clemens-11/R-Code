#########################################################################
#########################################################################
# Financial Stability indicators
#########################################################################
#########################################################################

### package
pacman::p_load(shiny)
pacman::p_load(shinythemes)
pacman::p_load(tidyverse)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(hrbrthemes)
pacman::p_load(ecb)
pacman::p_load(eurostat)
pacman::p_load(extrafont)
pacman::p_load(rmarkdown)
pacman::p_load(gridExtra)
pacman::p_load(grid)


data.path <- "C:/Users/MAS/Desktop/R Versuche/Financial_Stability"
code.path <- "/Data/"

#########################################################################
### Data for Non Performing loans
#########################################################################

dataset_NLP <- eurostat::get_eurostat("tipsbd10")

df_NLP <- dataset_NLP%>%
  mutate(year=as.double(paste0(substr(time,1,4))),
         country=geo,
         NLP_ratio=values)%>%
  dplyr::select(year,country,NLP_ratio)%>%
  filter(year>2000)

#########################################################################
### Data for Return on equity [%]
#########################################################################

source(paste(data.path, code.path, "Data_SDW_ECB_return_on_equity.R", sep = ""))

#########################################################################
### Data for Household debt in Austria
#########################################################################

dataset_debt_GDP <- eurostat::get_eurostat("nasa_10_f_bs")

data_debt_GDP <- dataset_debt_GDP%>%
  filter(unit %in% c("PC_GDP"),
         finpos %in% "LIAB",
         sector == "S14_S15",
         co_nco == "CO",
         na_item == "F4"
  )%>%
  mutate(year=as.double(paste0(substr(time,1,4))),
         country=geo,
         debt_GDP=values)%>%
  dplyr::select(year,country,debt_GDP,na_item)%>%
  filter(year>2000)

# Data for disposable income

dataset_dis_inc <- eurostat::get_eurostat("nasa_10_nf_tr")

data_dis_inc <- dataset_dis_inc%>%
  filter(unit %in% c("CP_MEUR"),
         direct %in% "PAID",
         sector == "S14_S15",
         na_item == "B6G")%>%
  mutate(year=as.double(paste0(substr(time,1,4))),
         country=geo,
         dis_inc=values)%>%
  dplyr::select(year,country,dis_inc)%>%
  filter(year>2000)

data_debt_GDP_mio <- dataset_debt_GDP%>%
  filter(unit %in% c("MIO_EUR"),
         finpos %in% "LIAB",
         sector == "S14_S15",
         co_nco == "CO",
         na_item %in% c("F4"))%>%
  mutate(year=as.double(paste0(substr(time,1,4))),
         country=geo,
         debt_mio=values)%>%
  dplyr::select(year,country,debt_mio)%>%
  filter(year>2000)

df_debt_hh <- data_dis_inc%>%
  left_join(data_debt_GDP_mio, by=c("year"="year","country"="country"))%>%
  mutate(debt_dis_inc=(debt_mio/dis_inc)*100)%>%
  left_join(data_debt_GDP, by=c("year"="year","country"="country"))%>%
  dplyr::select(year,country,debt_GDP,debt_dis_inc)%>%
  filter(year>2000&year<2020)%>%
  pivot_longer(!c(country,year), names_to = "type","value")
  


##############################################################################################
### Shiny App
##############################################################################################

### Define UI
countries <- unique(df_debt_hh$country)
countries2 <- unique(df_NLP$country)
countries3 <- unique(df_return_on_equity$area)
types <- unique(df_debt_hh$type)

shinyUI <- navbarPage(h2("Financial Stability"),
                      theme = shinytheme("darkly"),
                      tabPanel(h5("Household Debt"),
                               uiOutput('page1')),
                      tabPanel(h5("Non-performing Loans"),
                               uiOutput('page2')),
                      tabPanel(h5("Return on Equity"),
                               uiOutput('page3'))
                      )

### Define server
server <- function(input, output) {
  output$ui_länderwahl <- renderUI(
    checkboxGroupInput("länderwahl",
                       label="Country:",
                       countries,
                       selected=countries[1],
                       inline   = TRUE)
  )
  
  output$ui_type <- renderUI(
    checkboxGroupInput("typewahl",
                       label="Type:",
                       types,
                       selected=types[1])
  )
  
  output$ui_länderwahl2 <- renderUI(
    checkboxGroupInput("länderwahl2",
                       label="Country:",
                       countries2,
                       selected=countries2[1])
  )
  
  output$ui_länderwahl3 <- renderUI(
    checkboxGroupInput("länderwahl3",
                       label="Area:",
                       countries3,
                       selected=countries3[1])
  )
  
  output$chart_hh_debt <- renderPlot({
    df_debt_hh %>%
      filter(country %in% input$länderwahl,
             type %in% input$typewahl)%>%
      ggplot(aes(x=year,
                 y=value)) +
      geom_line(aes(color = country,
                linetype = type)) +
      coord_cartesian(xlim=input$slider)+
      scale_x_continuous(breaks = seq(2001,2021,1))+
      scale_y_continuous(breaks = seq(0,200, by=10))+
      scale_linetype_manual(values=c("solid", "dashed"))+
      guides(lty = guide_legend(override.aes = list(col = 'white')))+
      labs(title = "Household Debt", 
           subtitle = "% of Disposable income or GDP",
           y = "", x = "",
           caption = paste("Source: Eurostat",
                           format(Sys.Date(), format="%d %m %Y"), sep=" ")) + 
      hrbrthemes::theme_modern_rc()+
      theme(legend.title = element_blank(),
            legend.position = "top")
  })
  
  output$page1 <- renderUI({sidebarLayout(position = "left",
                sidebarPanel(
                  sliderInput("slider", label = "Year",
                              min = min(df_debt_hh$year),
                              max = max(df_debt_hh$year),
                              value=c(min(df_debt_hh$year),
                                      max(df_debt_hh$year)),
                              sep=""),
                  uiOutput("ui_länderwahl"),
                  uiOutput("ui_type")),
                mainPanel(plotOutput("chart_hh_debt",
                                     height = "600px",
                                     width = "800px"))
    )
  })

  output$chart_npl <- renderPlot({
    df_NLP %>% 
      filter(country %in% input$länderwahl2)%>%
      ggplot(aes(x=year,
                 y=NLP_ratio,
                 color = country)) +
      geom_line() +
      coord_cartesian(xlim=input$slider2)+
      scale_y_continuous(breaks = seq(0,50, by=2))+
      scale_x_continuous(breaks = seq(2001,2021,1))+
      labs(title = "Non-performing Loans", 
           subtitle = "",
           y = "", x = "",
           caption = paste("Source: Eurostat",
                           format(Sys.Date(), format="%d %m %Y"), sep=" ")) + 
      hrbrthemes::theme_modern_rc()+
      theme(legend.title = element_blank(),
            legend.position = "top")
  })
  
  output$page2 <- renderUI({sidebarLayout(position = "left",
                  sidebarPanel(
                  sliderInput("slider2", label = "Year",
                              min = 2014,
                              max = 2019,
                              value=c(2014,2019),
                              sep = ""),
                  uiOutput("ui_länderwahl2")),
                  mainPanel(plotOutput("chart_npl",
                                     height = "600px",
                                     width = "800px")))
  })
  
  output$chart_roe <- renderPlot({
    df_return_on_equity %>% 
      filter(area %in% input$länderwahl3)%>%
      ggplot(aes(x=year,
                 y=return_on_equity,
                 color = area)) +
      geom_line() +
      coord_cartesian(xlim=input$slider3)+
      scale_y_continuous(breaks = seq(-200,200, by=2))+
      scale_x_continuous(breaks = seq(2001,2021,1))+
      labs(title = "Return on Equity", 
           subtitle = "",
           y = "", x = "",
           caption = paste("Source: Eurostat",
                           format(Sys.Date(), format="%d %m %Y"), sep=" ")) + 
      hrbrthemes::theme_modern_rc()+
      theme(legend.title = element_blank(),
            legend.position = "top")
  })
  
  output$page3 <- renderUI({sidebarLayout(position = "left",
                                          sidebarPanel(
                                            sliderInput("slider3", label = "Year",
                                                        min = 2007,
                                                        max = 2020,
                                                        value=c(2007,2020),
                                                        sep = ""),
                                            uiOutput("ui_länderwahl3")),
                                          mainPanel(plotOutput("chart_roe",
                                                               height = "600px",
                                                               width = "800px")))
  })

}

### Run Shiny
shinyApp(shinyUI, server)

