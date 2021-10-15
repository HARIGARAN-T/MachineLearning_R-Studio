# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <-readRDS("RFM.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Google APP Rating Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("Reviews", 
                 label = "Reviews", 
                 value = 55),
    numericInput("Size", 
                 label = "Size", 
                 value =2700000),
    numericInput("Installs", 
                 label = "Installs", 
                 value = 5000),
    numericInput("Price", 
                 label = "Price", 
                 value = 0),
    selectInput("Content.Rating_c","Content.Rating_c",c("Adults only 18+","Everyone","Everyone 10+","Mature 17+","Teen","Unrated")),
    selectInput("Genres_c","Genres_c",c("Action", "Action;Action & Adventure" ,"Adventure" ,"Adventure;Action & Adventure", "Adventure;Brain Games", "Adventure;Education","Arcade", "Arcade;Action & Adventure", "Arcade;Pretend Play" ,"Art & Design" ,"Art & Design;Action & Adventure" ,"Art & Design;Creativity" , "Art & Design;Pretend Play", "Auto & Vehicles" ,"Beauty","Board","Board;Action & Adventure", "Board;Brain Games" , "Board;Pretend Play" ,"Books & Reference"  ,"Books & Reference;Creativity" ,"Books & Reference;Education","Business","Card","Card;Action & Adventure"  ,"Card;Brain Games" , "Casino" , "Casual" , "Casual;Action & Adventure" ,"Casual;Brain Games" ,"Casual;Creativity"  , "Casual;Education" ,"Casual;Music & Video"  ,"Casual;Pretend Play","Comics", "Comics;Creativity" ,"Communication"  , "Communication;Creativity","Dating",
                                        "Education" , "Education;Action & Adventure"  ,"Education;Brain Games"              ,"Education;Creativity" , "Education;Education" ,"Education;Music & Video","Education;Pretend Play" ,"Educational" ,"Educational;Action & Adventure" ,"Educational;Brain Games" , "Educational;Creativity", "Educational;Education" , "Educational;Pretend Play" , "Entertainment", "Entertainment;Action & Adventure" , "Entertainment;Brain Games"  ,"Entertainment;Creativity", "Entertainment;Education", "Entertainment;Music & Video" ,"Entertainment;Pretend Play" , "Events", "Finance" , "Food & Drink"  ,"Health & Fitness"  ,"Health & Fitness;Action & Adventure", "Health & Fitness;Education" ,"House & Home"  , "Libraries & Demo" , "Lifestyle" , "Lifestyle;Education" ,"Lifestyle;Pretend Play"  ,"Maps & Navigation" ,"Medical" , 
                                        "Music" , "Music & Audio;Music & Video" , "Music;Music & Video"  ,"News & Magazines"  , "Parenting" , "Parenting;Brain Games", "Parenting;Education"  ,"Parenting;Music & Video"  ,"Personalization"                     ,"Photography" , "Productivity" ,"Puzzle" , "Puzzle;Action & Adventure"  , "Puzzle;Brain Games" , "Puzzle;Creativity"  ,"Puzzle;Education" , "Racing"  ,"Racing;Action & Adventure"   ,"Racing;Pretend Play" ,"Role Playing"  ,"Role Playing;Action & Adventure" , "Role Playing;Brain Games" ,"Role Playing;Education"   , "Role Playing;Pretend Play" ,"Shopping"  , "Simulation"   ,"Simulation;Action & Adventure", "Simulation;Education"  , "Simulation;Pretend Play" , "Social" , "Sports"          ,"Sports;Action & Adventure"  , "Strategy" , "Strategy;Action & Adventure" ,"Strategy;Creativity" ,
                                        "Strategy;Education" , "Tools" ,"Tools;Education" , "Travel & Local" ,"Travel & Local;Action & Adventure" ,"Trivia" , "Trivia;Education"  ,"Video Players & Editors" , "Video Players & Editors;Creativity" , "Video Players & Editors;Music & Video", "Weather" ,"Word")  ) ,
    selectInput("Type_Free","Free or Paid",c("Free","Paid")),
    selectInput("Category_ART_AND_DESIGN","Category", c("ART_AND_DESIGN" ,"AUTO_AND_VEHICLES","BEAUTY", "BOOKS_AND_REFERENCE","BUSINESS","COMICS","COMMUNICATION" ,"DATING","EDUCATION","ENTERTAINMENT" ,"EVENTS","FAMILY","FINANCE","FOOD_AND_DRINK","GAME","HEALTH_AND_FITNESS","HOUSE_AND_HOME","LIBRARIES_AND_DEMO","LIFESTYLE","MAPS_AND_NAVIGATION", "MEDICAL","NEWS_AND_MAGAZINES","PARENTING","PERSONALIZATION","PHOTOGRAPHY","PRODUCTIVITY","SHOPPING","SOCIAL ", "SPORTS ","TOOLS","TRAVEL_AND_LOCAL","VIDEO_PLAYERS","WEATHER")),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Predicted Rating for APP')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    
    if(input$Type_Free == " Free"){
      x <- 1
    }
    else{
      x<-0
    }
    if(x==0){
      y <- 1
    }
    else{
      y <- 0
    }
    
    if (input$Content.Rating_c =="Adults only 18+" ){
      z <- 1
    }else if(input$Content.Rating_c =="Everyone" ){
      z <- 2
    }else if(input$Content.Rating_c =="Everyone 10+" ){
      z <- 3
    }else if(input$Content.Rating_c =="Mature 17+" ){
      z <- 4
    }else if(input$Content.Rating_c =="Teen" ){
      z <- 5
    }else {
      z <- 6
    }
    check <- c("Action", "Action;Action & Adventure" ,"Adventure" ,"Adventure;Action & Adventure", "Adventure;Brain Games", "Adventure;Education","Arcade", "Arcade;Action & Adventure", "Arcade;Pretend Play" ,"Art & Design" ,"Art & Design;Action & Adventure" ,"Art & Design;Creativity" , "Art & Design;Pretend Play", "Auto & Vehicles" ,"Beauty","Board","Board;Action & Adventure", "Board;Brain Games" , "Board;Pretend Play" ,"Books & Reference"  ,"Books & Reference;Creativity" ,"Books & Reference;Education","Business","Card","Card;Action & Adventure"  ,"Card;Brain Games" , "Casino" , "Casual" , "Casual;Action & Adventure" ,"Casual;Brain Games" ,"Casual;Creativity"  , "Casual;Education" ,"Casual;Music & Video"  ,"Casual;Pretend Play","Comics", "Comics;Creativity" ,"Communication"  , "Communication;Creativity","Dating",
                "Education" , "Education;Action & Adventure"  ,"Education;Brain Games"              ,"Education;Creativity" , "Education;Education" ,"Education;Music & Video","Education;Pretend Play" ,"Educational" ,"Educational;Action & Adventure" ,"Educational;Brain Games" , "Educational;Creativity", "Educational;Education" , "Educational;Pretend Play" , "Entertainment", "Entertainment;Action & Adventure" , "Entertainment;Brain Games"  ,"Entertainment;Creativity", "Entertainment;Education", "Entertainment;Music & Video" ,"Entertainment;Pretend Play" , "Events", "Finance" , "Food & Drink"  ,"Health & Fitness"  ,"Health & Fitness;Action & Adventure", "Health & Fitness;Education" ,"House & Home"  , "Libraries & Demo" , "Lifestyle" , "Lifestyle;Education" ,"Lifestyle;Pretend Play"  ,"Maps & Navigation" ,"Medical" , 
                "Music" , "Music & Audio;Music & Video" , "Music;Music & Video"  ,"News & Magazines"  , "Parenting" , "Parenting;Brain Games", "Parenting;Education"  ,"Parenting;Music & Video"  ,"Personalization"                     ,"Photography" , "Productivity" ,"Puzzle" , "Puzzle;Action & Adventure"  , "Puzzle;Brain Games" , "Puzzle;Creativity"  ,"Puzzle;Education" , "Racing"  ,"Racing;Action & Adventure"   ,"Racing;Pretend Play" ,"Role Playing"  ,"Role Playing;Action & Adventure" , "Role Playing;Brain Games" ,"Role Playing;Education"   , "Role Playing;Pretend Play" ,"Shopping"  , "Simulation"   ,"Simulation;Action & Adventure", "Simulation;Education"  , "Simulation;Pretend Play" , "Social" , "Sports"          ,"Sports;Action & Adventure"  , "Strategy" , "Strategy;Action & Adventure" ,"Strategy;Creativity" ,
                "Strategy;Education" , "Tools" ,"Tools;Education" , "Travel & Local" ,"Travel & Local;Action & Adventure" ,"Trivia" , "Trivia;Education"  ,"Video Players & Editors" , "Video Players & Editors;Creativity" , "Video Players & Editors;Music & Video", "Weather" ,"Word")
    
    for(l in 1:119){
      if(check[l] == input$Genres_c) {
        gen_c<-l
        break
      }
    }
    if(input$Category_ART_AND_DESIGN == "ART_AND_DESIGN")
    {
      
      ART_AND_DESIGN <- 1 
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0     
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0   
      COMICS  <-0           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    } else if (input$Category_ART_AND_DESIGN == "AUTO_AND_VEHICLES" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-1
      BEAUTY        <-0     
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0   
      COMICS  <-0           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "BEAUTY" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-1 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0   
      COMICS  <-0           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }  else if (input$Category_ART_AND_DESIGN =="BOOKS_AND_REFERENCE" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0
      BOOKS_AND_REFERENCE<-1
      BUSINESS<-0   
      COMICS  <-0           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }  else if (input$Category_ART_AND_DESIGN ==   "  BUSINESS" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-1
      COMICS  <-0           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }  else if (input$Category_ART_AND_DESIGN ==   "COMICS" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-1           
      COMMUNICATION <-0   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }  
    else if (input$Category_ART_AND_DESIGN ==   " COMMUNICATION" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-1   
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }
    else if (input$Category_ART_AND_DESIGN ==   "DATING" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-1
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "EDUCATION" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-0
      EDUCATION          <-1
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "ENTERTAINMENT" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-1
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "EVENTS" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-1
      FAMILY             <-0
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "FAMILY" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-1
      FINANCE          <-0
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "FINANCE" )
    {
      
      ART_AND_DESIGN <- 0
      AUTO_AND_VEHICLES <-0
      BEAUTY        <-0 
      BOOKS_AND_REFERENCE<-0
      BUSINESS<-0
      COMICS  <-0     
      COMMUNICATION <-0
      DATING           <-0
      EDUCATION          <-0
      ENTERTAINMENT  <-0
      EVENTS          <-0
      FAMILY             <-0
      FINANCE          <-1
      FOOD_AND_DRINK     <-0
      GAME               <-0
      HEALTH_AND_FITNESS<-0
      HOUSE_AND_HOME    <-0
      LIBRARIES_AND_DEMO <-0
      LIFESTYLE <-0
      MAPS_AND_NAVIGATION <-0
      MEDICAL  <-0
      NEWS_AND_MAGAZINES <-0
      PARENTING         <-0
      PERSONALIZATION    <-0
      PHOTOGRAPHY   <-0
      PRODUCTIVITY     <-0 
      SHOPPING           <-0
      SOCIAL         <-0
      SPORTS      <-0
      TOOLS          <-0    
      TRAVEL_AND_LOCAL  <-0
      VIDEO_PLAYERS     <-0
      WEATHER   <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "FOOD_AND_DRINK" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-1
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "GAME" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-1
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "HEALTH_AND_FITNESS" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-1
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "HOUSE_AND_HOME" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-1
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "LIBRARIES_AND_DEMO" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-1
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "LIFESTYLE" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-1
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "MAPS_AND_NAVIGATION" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-1
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "MEDICAL" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-1
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "NEWS_AND_MAGAZINES" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-1
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   " PARENTING" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-1
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "PERSONALIZATION" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-1
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "PHOTOGRAPHY" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-1
      PRODUCTIVITY         <-0 
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "PRODUCTIVITY" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-1
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "SHOPPING" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-1
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "SOCIAL" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-1
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "SPORTS" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-1
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "TOOLS" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-1    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "TRAVEL_AND_LOCAL" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-1
      VIDEO_PLAYERS        <-0
      WEATHER              <-0
      
    }else if (input$Category_ART_AND_DESIGN ==   "VIDEO_PLAYERS" )
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-1
      WEATHER              <-0
      
    }else 
    {
      
      ART_AND_DESIGN       <-0
      AUTO_AND_VEHICLES    <-0
      BEAUTY               <-0 
      BOOKS_AND_REFERENCE  <-0
      BUSINESS             <-0
      COMICS               <-0     
      COMMUNICATION        <-0
      DATING               <-0
      EDUCATION            <-0
      ENTERTAINMENT        <-0
      EVENTS               <-0
      FAMILY               <-0
      FINANCE              <-0
      FOOD_AND_DRINK       <-0
      GAME                 <-0
      HEALTH_AND_FITNESS   <-0
      HOUSE_AND_HOME       <-0
      LIBRARIES_AND_DEMO   <-0
      LIFESTYLE            <-0
      MAPS_AND_NAVIGATION  <-0
      MEDICAL              <-0
      NEWS_AND_MAGAZINES   <-0
      PARENTING            <-0
      PERSONALIZATION      <-0
      PHOTOGRAPHY          <-0
      PRODUCTIVITY         <-0
      SHOPPING             <-0
      SOCIAL               <-0
      SPORTS               <-0
      TOOLS                <-0    
      TRAVEL_AND_LOCAL     <-0
      VIDEO_PLAYERS        <-0
      WEATHER              <-1
      
    }
    df <- data.frame(
      Name = c("Reviews",
               "Size",
               "Installs",
               "Price","Content.Rating_c","Genres_c","Type_Free","Type_Paid","Category_ART_AND_DESIGN",
               "Category_AUTO_AND_VEHICLES","Category_BEAUTY","Category_BOOKS_AND_REFERENCE","Category_BUSINESS",
               "Category_COMICS","Category_COMMUNICATION","Category_DATING","Category_EDUCATION","Category_ENTERTAINMENT",
               "Category_EVENTS","Category_FAMILY","Category_FINANCE","Category_FOOD_AND_DRINK","Category_GAME","Category_HEALTH_AND_FITNESS",
               "Category_HOUSE_AND_HOME","Category_LIBRARIES_AND_DEMO","Category_LIFESTYLE","Category_MAPS_AND_NAVIGATION","Category_MEDICAL","Category_NEWS_AND_MAGAZINES",
               "Category_PARENTING","Category_PERSONALIZATION","Category_PHOTOGRAPHY","Category_PRODUCTIVITY","Category_SHOPPING","Category_SOCIAL","Category_SPORTS","Category_TOOLS",
               "Category_TRAVEL_AND_LOCAL","Category_VIDEO_PLAYERS","Category_WEATHER"),
      Value = c(input$Reviews,
                             input$Size,
                             input$Installs,
                             input$Price,z,gen_c,x,y,
                             ART_AND_DESIGN ,AUTO_AND_VEHICLES, BEAUTY  ,BOOKS_AND_REFERENCE  ,BUSINESS,      COMICS , COMMUNICATION  ,DATING ,EDUCATION ,ENTERTAINMENT, EVENTS , FAMILY ,FINANCE,
                             FOOD_AND_DRINK,GAME, HEALTH_AND_FITNESS,HOUSE_AND_HOME ,LIBRARIES_AND_DEMO,LIFESTYLE , MAPS_AND_NAVIGATION  ,MEDICAL , NEWS_AND_MAGAZINES,   PARENTING, PERSONALIZATION,
                             PHOTOGRAPHY , PRODUCTIVITY , SHOPPING ,SOCIAL, SPORTS,TOOLS ,TRAVEL_AND_LOCAL ,VIDEO_PLAYERS  ,WEATHER),
      stringsAsFactors = FALSE)
    
    Rating <- 0
    df <- rbind(df, Rating)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
