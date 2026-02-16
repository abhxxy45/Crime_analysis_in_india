# Load libraries
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

# Load dataset
crime=read.csv("data/india_crime_raw.csv")
colnames(crime)[1]="State"
crime=crime%>%filter(!grepl("Total",State))

# Growth calculation
crime$Growth_2020_2022=((crime$X2022-crime$X2020)/crime$X2020)*100

# Long format conversion
crime_long=crime%>%
  pivot_longer(cols=c(X2020,X2021,X2022),
               names_to="Year",
               values_to="Cases")%>%
  mutate(Year=gsub("X","",Year))

# UI
ui=dashboardPage(
  dashboardHeader(title="India Crime Analysis Dashboard"),
  dashboardSidebar(
    selectInput("year","Select Year",choices=c("2020","2021","2022")),
    selectInput("state","Select State",choices=unique(crime$State)),
    downloadButton("downloadData","Download Year Data")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("topState"),
      valueBoxOutput("avgCrime"),
      valueBoxOutput("totalCrime")
    ),
    fluidRow(
      box(width=12,leafletOutput("choropleth",height=500))
    ),
    fluidRow(
      box(width=6,plotOutput("trendPlot",height=400)),
      box(width=6,plotOutput("clusterPlot",height=400))
    ),
    fluidRow(
      box(width=6,plotOutput("ratePlot",height=500)),
      box(width=6,plotOutput("chargePlot",height=500))
    ),
    fluidRow(
      box(width=12,plotOutput("pieChart",height=400))
    ),
    fluidRow(
      box(width=6,verbatimTextOutput("prediction")),
      box(width=6,verbatimTextOutput("modelAccuracy"))
    )
  ))

# Server
server=function(input,output){
  
  selected_year_data=reactive({
    crime_long%>%filter(Year==input$year)
  })
  
  selected_state_data=reactive({
    crime_long%>%filter(State==input$state)%>%arrange(Year)
  })
  
  output$topState=renderValueBox({
    top=selected_year_data()%>%arrange(desc(Cases))%>%slice(1)
    valueBox(top$State,paste("Highest Crime -",input$year),
             icon=icon("exclamation-triangle"),color="red")
  })
  
  output$avgCrime=renderValueBox({
    avg=round(mean(selected_year_data()$Cases),0)
    valueBox(avg,"Average Crime",
             icon=icon("chart-line"),color="yellow")
  })
  
  output$totalCrime=renderValueBox({
    total=sum(selected_year_data()$Cases)
    valueBox(total,"Total Crime",
             icon=icon("database"),color="blue")
  })
  
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
  
  output$trendPlot=renderPlot({
    ggplot(selected_state_data(),
           aes(x=Year,y=Cases,group=1))+
      geom_line(color="blue",linewidth=1.3)+
      geom_point(size=3)+
      theme_minimal()+
      labs(title=paste("Crime Trend -",input$state),
           x="Year",y="Cases")
  })
  
  output$ratePlot=renderPlot({
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),
                         Rate.of.Cognizable.Crimes..IPC...2022.),
               y=Rate.of.Cognizable.Crimes..IPC...2022.))+
      geom_bar(stat="identity",fill="darkred")+
      coord_flip()+theme_minimal()+
      labs(title="Crime Rate per Lakh (2022)",
           x="State",y="Rate")
  })
  
  output$chargePlot=renderPlot({
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),
                         Chargesheeting.Rate..2022.),
               y=Chargesheeting.Rate..2022.))+
      geom_bar(stat="identity",fill="darkblue")+
      coord_flip()+theme_minimal()+
      labs(title="Chargesheeting Rate (2022)",
           x="State",y="Rate (%)")
  })
  
  output$clusterPlot=renderPlot({
    cluster_data=crime%>%select(X2022)
    set.seed(123)
    km=kmeans(cluster_data,centers=3)
    crime$Cluster=as.factor(km$cluster)
    ggplot(crime,
           aes(x=reorder(str_wrap(State,20),X2022),
               y=X2022,fill=Cluster))+
      geom_bar(stat="identity")+
      coord_flip()+theme_minimal()+
      labs(title="Crime Clusters (2022)",
           x="State",y="Cases")
  })
  
  output$pieChart=renderPlot({
    pie_data=selected_year_data()%>%
      arrange(desc(Cases))%>%
      head(5)
    ggplot(pie_data,aes(x="",y=Cases,fill=State))+
      geom_bar(stat="identity",width=1)+
      coord_polar("y")+theme_void()+
      labs(title=paste("Top 5 Crime Share -",input$year))
  })
  
  output$prediction=renderPrint({
    ts_data=ts(selected_state_data()$Cases,start=2020)
    model=auto.arima(ts_data)
    forecast(model,h=1)
  })
  
  output$modelAccuracy=renderPrint({
    ts_data=ts(selected_state_data()$Cases,start=2020)
    model=auto.arima(ts_data)
    accuracy(model)
  })
  
  output$downloadData=downloadHandler(
    filename=function(){paste("Crime_Data_",input$year,".csv",sep="")},
    content=function(file){
      write.csv(selected_year_data(),file,row.names=FALSE)
    }
  )
  
}

shinyApp(ui,server)
