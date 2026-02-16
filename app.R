# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(sf)
library(forecast)
library(rnaturalearth)
library(rnaturalearthhires)
library(stringr)
library(shinyjs)
library(shinycssloaders)

# Read and clean dataset
crime=read.csv("data/india_crime_raw.csv")
colnames(crime)[1]="State"
crime=crime%>%filter(!grepl("Total",State))

# Calculate growth rate
crime$Growth_2020_2022=((crime$X2022-crime$X2020)/crime$X2020)*100

# Convert wide to long format
crime_long=crime%>%
  pivot_longer(cols=c(X2020,X2021,X2022),
               names_to="Year",
               values_to="Cases")%>%
  mutate(Year=gsub("X","",Year))

# Create dashboard UI
ui=dashboardPage(
  skin="black",
  dashboardHeader(title="India Crime Analysis"),
  
  dashboardSidebar(
    selectInput("year","📅 Select Year",choices=c("2020","2021","2022")),
    selectInput("state","📍 Select State",choices=unique(crime$State)),
    downloadButton("downloadData","⬇ Download Year Data")
  ),
  
  dashboardBody(
    
    useShinyjs(),
    
    tags$head(tags$style(HTML("
.content-wrapper {
background: linear-gradient(135deg, #1f1c2c, #4a148c, #16222A);
color: white;
}
.box {
background: rgba(255,255,255,0.08);
backdrop-filter: blur(14px);
border-radius: 18px;
border: 1px solid rgba(255,255,255,0.2);
box-shadow: 0 8px 32px rgba(0,0,0,0.37);
color: white;
}
.main-sidebar {
background: rgba(0,0,0,0.6) !important;
}
"))),
    
    h3("Interactive Crime Dashboard (2020-2022)",align="center"),
    
    # Value boxes
    fluidRow(
      valueBoxOutput("topState"),
      valueBoxOutput("avgCrime"),
      valueBoxOutput("totalCrime"),
      valueBoxOutput("growthBox")
    ),
    
    # Map
    fluidRow(
      box(width=12,withSpinner(leafletOutput("choropleth",height=500)))
    ),
    
    # Trend plot
    fluidRow(
      box(width=12,withSpinner(plotOutput("trendPlot",height=500)))
    ),
    
    # Insight section
    fluidRow(
      box(width=12,
          h4("📊 Automated Insight"),
          verbatimTextOutput("insightText"))
    ),
    
    # Cluster plot
    fluidRow(
      box(width=12,withSpinner(plotOutput("clusterPlot",height=400)))
    ),
    
    # Rate comparison
    fluidRow(
      box(width=6,withSpinner(plotOutput("ratePlot",height=500))),
      box(width=6,withSpinner(plotOutput("chargePlot",height=500)))
    ),
    
    # Correlation & histogram
    fluidRow(
      box(width=6,withSpinner(plotOutput("correlationPlot",height=400))),
      box(width=6,withSpinner(plotOutput("histogramPlot",height=400)))
    ),
    
    # Pie chart
    fluidRow(
      box(width=12,withSpinner(plotOutput("pieChart",height=400)))
    ),
    
    # Forecast outputs
    fluidRow(
      box(width=6,verbatimTextOutput("prediction")),
      box(width=6,verbatimTextOutput("modelAccuracy"))
    )
    
  ))

# Server logic
server=function(input,output){
  
  selected_year_data=reactive({
    crime_long%>%filter(Year==input$year)
  })
  
  selected_state_data=reactive({
    crime_long%>%filter(State==input$state)%>%arrange(Year)
  })
  
  # Top state box
  output$topState=renderValueBox({
    top=selected_year_data()%>%arrange(desc(Cases))%>%slice(1)
    valueBox(top$State,paste("Highest Crime -",input$year),
             icon=icon("exclamation-triangle"),color="red")
  })
  
  # Average crime box
  output$avgCrime=renderValueBox({
    avg=round(mean(selected_year_data()$Cases),0)
    valueBox(avg,"Average Crime",
             icon=icon("chart-line"),color="yellow")
  })
  
  # Total crime box
  output$totalCrime=renderValueBox({
    total=sum(selected_year_data()$Cases)
    valueBox(total,"Total Crime",
             icon=icon("database"),color="blue")
  })
  
  # Growth box
  output$growthBox=renderValueBox({
    growth=crime%>%filter(State==input$state)%>%pull(Growth_2020_2022)
    valueBox(paste(round(growth,2),"%"),
             "Growth 2020-22",
             icon=icon("chart-line"),
             color=ifelse(growth>0,"green","red"))
  })
  
  # Choropleth map
  output$choropleth=renderLeaflet({
    india_states=ne_states(country="India",returnclass="sf")
    data_map=crime_long%>%filter(Year==input$year)
    india_states=india_states%>%left_join(data_map,by=c("name"="State"))
    pal=colorNumeric("Reds",domain=india_states$Cases,na.color="white")
    leaflet(india_states)%>%
      addTiles()%>%
      addPolygons(fillColor=~pal(Cases),
                  weight=1,color="white",fillOpacity=0.8,
                  popup=~paste(name,"<br>Cases:",Cases))%>%
      addLegend(pal=pal,values=~Cases,title="Crime Cases")
  })
  
  # Trend plot with labels
  output$trendPlot=renderPlot({
    ggplot(selected_state_data(),
           aes(x=Year,y=Cases,group=1))+
      geom_line(color="#00e5ff",linewidth=1.5)+
      geom_point(size=4,color="#ff4081")+
      geom_text(aes(label=Cases),
                vjust=-1,
                color="black",
                size=5,
                fontface="bold")+
      theme_minimal()+
      labs(title=paste("Crime Trend -",input$state),
           x="Year",y="Cases")
  })
  
  # 🔥 Automated Insight
  output$insightText=renderPrint({
    
    data_state=selected_state_data()
    
    if(nrow(data_state)>=2){
      
      first=data_state$Cases[1]
      last=data_state$Cases[nrow(data_state)]
      
      growth_percent=round(((last-first)/first)*100,2)
      
      trend=ifelse(growth_percent>0,"increased",
                   ifelse(growth_percent<0,"decreased","remained stable"))
      
      cat("In",input$state,", crime has",trend,
          "by",abs(growth_percent),"% from",
          data_state$Year[1],"to",
          data_state$Year[nrow(data_state)],".\n\n")
      
      if(growth_percent>0){
        cat("The overall trend is upward, indicating rising crime levels.")
      } else if(growth_percent<0){
        cat("The overall trend is downward, indicating improvement.")
      } else {
        cat("Crime levels remained relatively stable during this period.")
      }
    }
  })
  
  # Cluster plot
  output$clusterPlot=renderPlot({
    cluster_data=crime%>%select(X2022)
    set.seed(123)
    km=kmeans(cluster_data,centers=3)
    crime$Cluster=as.factor(km$cluster)
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),X2022),
               y=X2022,fill=Cluster))+
      geom_bar(stat="identity")+
      coord_flip()+theme_minimal()
  })
  
  # Rate plot
  output$ratePlot=renderPlot({
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),
                         Rate.of.Cognizable.Crimes..IPC...2022.),
               y=Rate.of.Cognizable.Crimes..IPC...2022.))+
      geom_bar(stat="identity",fill="#ff6f00")+
      coord_flip()+theme_minimal()
  })
  
  # Chargesheet plot
  output$chargePlot=renderPlot({
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),
                         Chargesheeting.Rate..2022.),
               y=Chargesheeting.Rate..2022.))+
      geom_bar(stat="identity",fill="#00e676")+
      coord_flip()+theme_minimal()
  })
  
  # Correlation plot
  output$correlationPlot=renderPlot({
    ggplot(crime,
           aes(x=Rate.of.Cognizable.Crimes..IPC...2022.,
               y=X2022))+
      geom_point(color="#00e5ff",size=3)+
      geom_smooth(method="lm",color="#ff4081",se=FALSE)+
      theme_minimal()
  })
  
  # Histogram
  output$histogramPlot=renderPlot({
    ggplot(selected_year_data(),aes(x=Cases))+
      geom_histogram(bins=10,fill="#7c4dff",color="white")+
      theme_minimal()
  })
  
  # Pie chart
  output$pieChart=renderPlot({
    pie_data=selected_year_data()%>%
      arrange(desc(Cases))%>%head(5)
    ggplot(pie_data,aes(x="",y=Cases,fill=State))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y")+theme_void()
  })
  
  # Forecast
  output$prediction=renderPrint({
    ts_data=ts(selected_state_data()$Cases,start=2020)
    model=auto.arima(ts_data)
    forecast(model,h=1)
  })
  
  # Accuracy
  output$modelAccuracy=renderPrint({
    ts_data=ts(selected_state_data()$Cases,start=2020)
    model=auto.arima(ts_data)
    accuracy(model)
  })
  
  # Download handler
  output$downloadData=downloadHandler(
    filename=function(){
      paste("Crime_Data_",input$year,".csv",sep="")
    },
    content=function(file){
      write.csv(selected_year_data(),file,row.names=FALSE)
    }
  )
  
}

shinyApp(ui,server)
