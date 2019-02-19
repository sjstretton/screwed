#Before running, run >Session>Set Working Directory>To Source Files Location
rm(list=ls())
options(stringsAsFactors = FALSE)
options(scipen = 999) #remove scientific notation
#setwd("~/Google Drive/RTW/05-R Modelling")
library(shiny); 
library(scales); 
library(ineq); 
library(MASS); 
library(formattable); 
library(VGAM)

source("Model/Calc_1SimplifiedTaxFunctions.R")
source("Model/Calc_2DefineTaxSystems.R")
source("Model/Calc_3ConstructPopulation.R")
#source("Model/Calc_4DoAnalysis.R")

Individual <- Individual1
Individual[,"Carbon"]=10
InUseTaxSystem <- ExistingTaxSystem

server <- function(input, output) {
  UpdateIncomeTaxSystem <- function(InUseTaxSystem=InUseTaxSystem, input=input) {
    InUseTaxSystem$nBands=input$nBands
    InUseTaxSystem$IncTax=ExistingTaxSystem$IncTax[1:input$nBands,]
    InUseTaxSystem$IncTax[1,"LowerLimit"]=0
    if(input$nBands>1) InUseTaxSystem$IncTax[2,"LowerLimit"]=input$startOfBand2
    if(input$nBands>2) InUseTaxSystem$IncTax[3,"LowerLimit"]=input$startOfBand3
    if(input$nBands>3) InUseTaxSystem$IncTax[4,"LowerLimit"]=input$startOfBand4
    if(input$nBands>4) InUseTaxSystem$IncTax[5,"LowerLimit"]=input$startOfBand5
    if(input$nBands>5) InUseTaxSystem$IncTax[6,"LowerLimit"]=input$startOfBand6
    
    if(input$nBands>0) InUseTaxSystem$IncTax[1,"Rate"]=input$incomeTaxRate1
    if(input$nBands>1) InUseTaxSystem$IncTax[2,"Rate"]=input$incomeTaxRate2
    if(input$nBands>2) InUseTaxSystem$IncTax[3,"Rate"]=input$incomeTaxRate3
    if(input$nBands>3) InUseTaxSystem$IncTax[4,"Rate"]=input$incomeTaxRate4
    if(input$nBands>4) InUseTaxSystem$IncTax[5,"Rate"]=input$incomeTaxRate5
    if(input$nBands>5) InUseTaxSystem$IncTax[6,"Rate"]=input$incomeTaxRate6
    
    if(input$nBands>1) InUseTaxSystem$IncTax[1,"UpperLimit"]=input$startOfBand2
    if(input$nBands>2) InUseTaxSystem$IncTax[2,"UpperLimit"]=input$startOfBand3
    if(input$nBands>3) InUseTaxSystem$IncTax[3,"UpperLimit"]=input$startOfBand4
    if(input$nBands>4) InUseTaxSystem$IncTax[4,"UpperLimit"]=input$startOfBand5
    if(input$nBands>5) InUseTaxSystem$IncTax[5,"UpperLimit"]=input$startOfBand6
    
    InUseTaxSystem$IncTax$TaxPaidAtBottomOfBand <- c(0,cumsum((InUseTaxSystem$IncTax$UpperLimit-InUseTaxSystem$IncTax$LowerLimit)*InUseTaxSystem$IncTax$Rate)[-InUseTaxSystem$nBands])
    
    InUseTaxSystem$Property=input$propertyTaxRate/100
    InUseTaxSystem$Carbon=input$carbonTaxRate
    InUseTaxSystem$CitDiv=-input$CitDiv
    InUseTaxSystem$VAT=input$VAT/100
    return(InUseTaxSystem)
  }
  
  UpdateIndividual <- function(Individual, input) {
    Individual[,c("GrossIncome","IncTax","EmployeeNI","EmployerNI")]=input$GrossIncome
    Individual[,c("Property")]=input$Property
    Individual[,c("Carbon")]=input$Carbon
    return(Individual)
  }
  
  revenueSummary=function(input, TaxSystem, useExisting=FALSE, populationSwitch=FALSE) {
    if(useExisting) InUseTaxSystem = ExistingTaxSystem else InUseTaxSystem = UpdateIncomeTaxSystem(InUseTaxSystem, input)
    
    if(!populationSwitch) {
      Individual = UpdateIndividual(Individual, input)
      individualAggregateRevenue = aggregateRevenue(Individual,InUseTaxSystem)[,1:2]
    } else {
      individualAggregateRevenue = aggregateRevenue(PopulationCalc, InUseTaxSystem, scale=1e-6, IncludeBeforeAndAfter=FALSE, TaxesNegative=FALSE)[,1:2]
    }
    
    individualAggregateRevenue[,2] = as.character(accounting(individualAggregateRevenue[,2], digits = 0L))
    colnames(individualAggregateRevenue) <- NULL
    individualAggregateRevenue
  }
  
  taxSystemComparisonTable=function(input, populationSwitch=FALSE) {
    individualNewAndExistingTable=cbind(revenueSummary(input, useExisting=TRUE,populationSwitch=populationSwitch), New=revenueSummary(input,populationSwitch=populationSwitch)[,2])
    colnames(individualNewAndExistingTable) = c("Tax Type", "Current", "New")
    individualNewAndExistingTable
  }  
  
  output$IndividualSummaryTableOutput <- renderTable({
    taxSystemComparisonTable(input)
  })
  
  output$PopulationSummaryTableOutput <- renderTable({
    taxSystemComparisonTable(input, populationSwitch=TRUE)
  })
  
  
  plotIndividual=function(input) {
    InUseTaxSystem = UpdateIncomeTaxSystem(InUseTaxSystem, input) 
    Individual = UpdateIndividual(Individual, input)
    waterfall(aggregateRevenue(Individual,InUseTaxSystem,aggLevels=c("Cat3","Cat3"),scale = 1))
  }
  
  output$IndividualWaterfall <- renderPlot({
    plotIndividual(input)
  })
  
  plotPopulation=function(input) {
    InUseTaxSystem = UpdateIncomeTaxSystem(InUseTaxSystem, input) 
    Individual = UpdateIndividual(PopulationCalc, input)
    x1 = (aggregateRevenue(PopulationCalc,InUseTaxSystem,aggLevels=c("Cat2","Cat3"),scale = 1e-6,IncludeBeforeAndAfter=FALSE,TaxesNegative=FALSE))
    x2 = (aggregateRevenue(PopulationCalc,ExistingTaxSystem,aggLevels=c("Cat2","Cat3"),scale = 1e-6,IncludeBeforeAndAfter=FALSE,TaxesNegative=FALSE))
    x1$system="New"
    x2$system="Existing"
    x=rbind(x1,x2)
    colnames(x)[colnames(x)=="value"]<- "RevenueRaised"
    colnames(x)[colnames(x)=="system"]<- "TaxSystem"
    colnames(x)[colnames(x)=="category"]<- "TaxCategory"
    charts.data=x
    ggplot() + geom_bar(aes(y = RevenueRaised, x = TaxSystem, fill = TaxCategory), data = charts.data, stat="identity")
  }
  
  output$PopulationGraph <- renderPlot({
    plotPopulation(input)
  })
  
  
}

ui <- fluidPage(
  titlePanel("Tax Calculator"),
  
  #tabPanel("Tax System",
  sidebarLayout(
    mainPanel(
      tabsetPanel(
        tabPanel("Individual Result",
                 h3("Enter Your Details"),
                 fluidPage(
                   fluidRow(
                     column(4,numericInput("GrossIncome", 
                                           "Annual Income", Individual[,"GrossIncome"],step = 5000)
                     ),
                     column(4,numericInput("Property", 
                                           "House Value", Individual[,"Property"],step = 25000)
                     ),
                     column(4,numericInput("Carbon", 
                                           "Emissions (tCO2)", Individual[,"Carbon"],step = 1)
                     )
                   ),
                   fluidRow(
                     column(5,
                            h3("Results (£)"),
                            tableOutput('IndividualSummaryTableOutput')
                     ),
                     column(7,plotOutput('IndividualWaterfall')
                     )
                   )
                 )
        ),
        
        tabPanel("Population Result",
                 h3("Outcome For Population (£ bn)"),
                 fluidPage(
                   fluidRow(
                     column(6,
                            tableOutput('PopulationSummaryTableOutput')
                     ),
                     column(6,plotOutput('PopulationGraph')
                            
                     )
                   )
                 )
        )
      )
    ),
    
    
    sidebarPanel(h3("Define Tax System"),
      tabsetPanel(
        tabPanel("Various",
                 sliderInput(
                   inputId = "CitDiv",
                   label = "Yearly basic income (£)",
                   value = InUseTaxSystem$CitDiv,
                   min = 0,
                   max = 10000,
                   step = 500
                 ),
                 sliderInput(
                   inputId = "propertyTaxRate",
                   label = "Yearly LVT (% of house price)",
                   value = InUseTaxSystem$Property*100,
                   min = 0,
                   max = 5,
                   step = 0.1
                 ),
                 
                 sliderInput(
                   inputId = "carbonTaxRate",
                   label = "Carbon tax (£ per tonne CO2)",
                   value = InUseTaxSystem$Carbon,
                   min = 0,
                   max = 400,
                   step = 25
                 ),
                 
                 sliderInput(
                   inputId = "VAT",
                   label = "VAT Rate (%)",
                   value = InUseTaxSystem$VAT*100,
                   min = 0,
                   max = 50,
                   step = 1
                 )
        ),
        tabPanel("Income Tax",
                 sliderInput("nBands",
                             "Number of tax bands:",
                             min = 1,
                             max = 6,
                             value = InUseTaxSystem$nBands),
                 fluidPage(
                   fluidRow(
                     column(6,h5("Starting Income (£)")),
                     column(6,h5("Rate"))
                     
                   ),
                   
                   fluidRow(
                     
                     column(6,
                            p(0) ),
                     column(6,
                            sliderInput(
                              inputId = "incomeTaxRate1",
                              label = NULL,
                              value = InUseTaxSystem$IncTax[1,"Rate"],
                              min = 0,
                              max = 0.6,
                              step = 0.01
                            ))
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 1 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand2",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[2,"LowerLimit"],
                                min = 0,
                                max = 200000,
                                step = 2000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate2",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[2,"Rate"],
                                min = 0,
                                max = 0.6,
                                step = 0.01
                              ))
                       
                     )
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 2 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand3",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[3,"LowerLimit"],
                                min = 0,
                                max = 200000,
                                step = 5000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate3",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[3,"Rate"],
                                min = 0,
                                max = 1,
                                step = 0.01
                              ))
                       
                     )
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 3 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand4",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[4,"LowerLimit"],
                                min = 0,
                                max = 500000,
                                step = 5000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate4",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[4,"Rate"],
                                min = 0,
                                max = 1,
                                step = 0.01
                              ))
                       
                     )
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 4 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand5",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[5,"LowerLimit"],
                                min = 0,
                                max = 500000,
                                step = 5000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate5",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[5,"Rate"],
                                min = 0,
                                max = 1,
                                step = 0.01
                              ))
                       
                     )
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 5 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand6",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[6,"LowerLimit"],
                                min = 0,
                                max = 1000000,
                                step = 5000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate6",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[6,"Rate"],
                                min = 0,
                                max = 1,
                                step = 0.01
                              ))
                       
                     )
                   ),
                   conditionalPanel(
                     condition = "input.nBands > 6 ",
                     fluidRow(
                       column(6,
                              sliderInput(
                                inputId = "startOfBand7",
                                label = NULL,
                                value = InUseTaxSystem$IncTax[7,"LowerLimit"],
                                min = 0,
                                max = 1000000,
                                step = 5000
                              ) ),
                       column(6,
                              sliderInput(
                                inputId = "incomeTaxRate7",
                                label = NULL,
                                value = 0.60,
                                min = 0,
                                max = 1,
                                step = 0.01
                              ) 
                       )
                       
                     )
                   )
                 )
        )         
      )
    )
  ) 
)


shinyApp(ui = ui, server = server)