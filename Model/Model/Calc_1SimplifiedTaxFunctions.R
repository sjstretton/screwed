#FUNCTIONS

aggregationLookup=read.csv("TaxCategories.csv",colClasses=c("character","factor","factor","factor"))
aggregationLookup$Cat1=factor(aggregationLookup$Cat1,levels=c("GrossIncome","IT","NI", "VAT", "Fuel", "Sin", "Property","Other", "Benefit","PostTaxIncome"))
aggregationLookup$Cat2=factor(aggregationLookup$Cat2,levels=c("GrossIncome","DirectTax","IndirectTax", "PropertyTax","Benefit","PostTaxIncome"))
aggregationLookup$Cat3=factor(aggregationLookup$Cat3,levels=c("GrossIncome","Tax","Benefit","PostTaxIncome"))


#This function constructs population data By looking up average carbon emissions (etc) according to income decile and then copying income into columns with names of target tax (e.g. EmployeeNI).
ConstructPopulationData=function(Income,HouseholdData,Name=NULL,Pop=NULL,PropertyInput=NULL) {
  OutputDF <- HouseholdData[findInterval(Income, c(0,HouseholdData$Decilepoints[1:9])),c(-(1:3))]
  if(!is.null(PropertyInput)) OutputDF$Property=PropertyInput
  if(is.null(Name)) Name <- seq_along(Income)
  if(is.null(Pop)) Pop <- rep(1,length(Income))
  OutputDF <- cbind(Name, Pop, GrossIncome=Income, IncTax=Income, EmployeeNI=Income,EmployerNI=Income,CitDiv=rep(1,length(Income)),OutputDF)
  OutputDF  
}

#Creates an Income Tax Schedule with a set number of 
CreateIncTaxSchedule=function(BandName, Rate, LowerLimit) {
  TaxSchedule <- data.frame(BandName=BandName,Rate=Rate, LowerLimit=LowerLimit, UpperLimit=c(LowerLimit[-1],Inf))
  NumberOfBands <- length(TaxSchedule$BandName)
  TaxSchedule$TaxPaidAtBottomOfBand <- c(0,cumsum((TaxSchedule$UpperLimit-TaxSchedule$LowerLimit)*TaxSchedule$Rate)[-NumberOfBands])
  return(TaxSchedule)
}

#Creates a tax system with both income tax bands, indirect tax rates
CreateFullTaxSystem = function(IncTax,EmployeeNI,EmployerNI,IndirectTaxes,Benefits){
  nBands=nrow(IncTax)
return(c(list(nBands=nBands,IncTax=IncTax,EmployeeNI=EmployeeNI,EmployerNI=EmployerNI),IndirectTaxes,Benefits))
}

PreTaxIncomeModelledFunction <- function(LogNormDetails, n=42000) {
  set.seed(1)
  IncomeDistModelled <- rlnorm(n, meanlog = LogNormDetails$estimate["meanlog"],
                             sdlog = LogNormDetails$estimate["sdlog"])
  SortedIncome <- sort(IncomeDistModelled)
  return(SortedIncome)
}


TaxCalc = function(Quantity,TaxSchedule) {
  if(is.null(dim(TaxSchedule))) { #If there is just a rate
    return(TaxSchedule*Quantity) 
  } else { #If there is a schedule of rates
    TaxBand=pmax(1,findInterval(Quantity,TaxSchedule[,"LowerLimit"]))
    TotalTaxPaid <- TaxSchedule[TaxBand,"TaxPaidAtBottomOfBand"]+TaxSchedule[TaxBand,"Rate"]*(Quantity-TaxSchedule[TaxBand,"LowerLimit"])
    return(TotalTaxPaid)
  }
}


AllTaxCalc <- function(Population,TaxSystem) {
  CommonNames=intersect(names(TaxSystem),colnames(Population))
  TaxPaid=matrix(0,ncol=length(CommonNames),nrow=nrow(Population))
  colnames(TaxPaid)=CommonNames
  for(TaxName in CommonNames){
    TaxPaid[,TaxName] = TaxCalc(Population[,TaxName],TaxSystem[[TaxName]]) 
  }
  return(TaxPaid)
}

AllAnalytics <- function(Population,TaxSystem) {
  IncomeAndTaxesPaid=AllIncomeAndTaxCalc(Population,TaxSystem)
  SummaryResult=CalculateSummary(IncomeAndTaxesPaid)
  RevenueRaisedByTax=-colSums(IncomeAndTaxesPaid)[-1]
  TotalSummary=colSums(SummaryResult)
  
  if(nrow(Population)>10) {
  PreTaxGini=ineq(SummaryResult[,"GrossIncome"],"Gini")
  PostTaxGini=ineq(SummaryResult[,"PostAllTaxAndBen"],"Gini")
  PreTaxShareOfIncomePerQs=ShareOfIncomePerQs(SummaryResult[,"GrossIncome"])
  PostTaxShareOfIncomePerQs=ShareOfIncomePerQs(SummaryResult[,"PostAllTaxAndBen"])
  IneqList=list(PreTaxGini=PreTaxGini,PostTaxGini=PostTaxGini,PreTaxShareOfIncomePerQs=PreTaxShareOfIncomePerQs,PostTaxShareOfIncomePerQs=PostTaxShareOfIncomePerQs)
  } else {PreTaxGini=NA;PostTaxGini=NA;PreTaxShareOfIncomePerQs=NA;PostTaxShareOfIncomePerQs=NA}
  list(Population=Population,TaxSystemName=TaxSystem["TaxSystemName"],TaxSystem=TaxSystem,IncomeAndTaxesPaid=IncomeAndTaxesPaid,SummaryResult=SummaryResult,RevenueRaisedByTax=RevenueRaisedByTax,
       TotalSummary=TotalSummary,GrossIncome=TotalSummary["GrossIncome"],RevenueRaised=TotalSummary["RevenueRaised"], PostAllTaxAndBenIncome=TotalSummary["PostAllTaxAndBen"],
       PreTaxGini=PreTaxGini,PostTaxGini=PostTaxGini,PreTaxShareOfIncomePerQs=PreTaxShareOfIncomePerQs,PostTaxShareOfIncomePerQs=PostTaxShareOfIncomePerQs)
}

AllIncomeAndTaxCalc <- function(Population,TaxSystem) {
  AllTaxC=AllTaxCalc(Population,TaxSystem)
  cbind(GrossIncome=Population[,"GrossIncome"],-AllTaxC)
  }

CalculateSummaryFromStart <- function(Population,TaxSystem) {
  CalculateSummary(AllIncomeAndTaxCalc(Population,TaxSystem))
  }

CalculateSummary <- function(AllIncomeAndTaxCalcRes) {
  IncomeTax=intersect(colnames(AllIncomeAndTaxCalcRes),c("IncTax"))
  NI=intersect(colnames(AllIncomeAndTaxCalcRes),c("EmployeeNI","EmployerNI"))
  IncomeTaxAndNI=intersect(colnames(AllIncomeAndTaxCalcRes),c("IncTax","EmployeeNI","EmployerNI"))
  Benefits=intersect(colnames(AllIncomeAndTaxCalcRes),c("CitDiv"))
  PropertyTaxes=intersect(colnames(AllIncomeAndTaxCalcRes),c("Property"))
  IndirectTaxes=intersect(colnames(AllIncomeAndTaxCalcRes),c("VAT","Tobacco","Beer","Wine","Hydrocarbons","CustomsGoods","Betting","Insurance","Lottery","AirTravel","TvLicence","Carbon"))
  
  
  SummTable=cbind(GrossIncome=AllIncomeAndTaxCalcRes[,"GrossIncome"],
                  IncTax=rowSums(AllIncomeAndTaxCalcRes[,IncomeTax,drop=FALSE]),
                  NI=rowSums(AllIncomeAndTaxCalcRes[,NI,drop=FALSE]),
                  IncTaxAndNI=rowSums(AllIncomeAndTaxCalcRes[,IncomeTaxAndNI,drop=FALSE]),
                  Benefits=rowSums(AllIncomeAndTaxCalcRes[,Benefits,drop=FALSE]),
                  PropertyTaxes=rowSums(AllIncomeAndTaxCalcRes[,PropertyTaxes,drop=FALSE]),
                  IndirectTaxes=rowSums(AllIncomeAndTaxCalcRes[,IndirectTaxes,drop=FALSE]))
  
  SummTable=cbind(SummTable,
                  PostIncTax=(SummTable[,"GrossIncome"]+SummTable[,"IncTax"]),
                  PostIncTaxAndNI=(SummTable[,"GrossIncome"]+SummTable[,"IncTax"]+AllIncomeAndTaxCalcRes[,"EmployeeNI"]),
                  PostIncTaxAndNIAndBen=(SummTable[,"GrossIncome"]+SummTable[,"IncTax"]+AllIncomeAndTaxCalcRes[,"EmployeeNI"]+SummTable[,"Benefits"]),
                  PostAllTaxAndBen=(SummTable[,"GrossIncome"]+SummTable[,"IncTax"]+AllIncomeAndTaxCalcRes[,"EmployeeNI"]+AllIncomeAndTaxCalcRes[,"EmployerNI"]+SummTable[,"Benefits"]+SummTable[,"PropertyTaxes"]+SummTable[,"IndirectTaxes"]),
                  RevenueRaised=-(SummTable[,"IncTax"]+AllIncomeAndTaxCalcRes[,"EmployeeNI"]+AllIncomeAndTaxCalcRes[,"EmployerNI"]+SummTable[,"Benefits"]+SummTable[,"PropertyTaxes"]+SummTable[,"IndirectTaxes"]))

    return(SummTable)
}

TotalRevenueRaised=function(Population,TaxSystem,IncludeBeforeAndAfter=FALSE,TaxesNegative=FALSE) {
  TaxRevenueRaised=colSums(AllTaxCalc(Population,TaxSystem))
  if(!IncludeBeforeAndAfter) {
    if(TaxesNegative) return(-TaxRevenueRaised) else return(TaxRevenueRaised) 
    } else {
    GrossIncome=sum(Population$GrossIncome)
    PostTaxIncome=GrossIncome-sum(TaxRevenueRaised)
    if(TaxesNegative) return(c(GrossIncome=GrossIncome,-TaxRevenueRaised,PostTaxIncome=PostTaxIncome)) else return(c(GrossIncome=GrossIncome,TaxRevenueRaised,PostTaxIncome=PostTaxIncome))
  }
}

ShareOfIncomePerQs <- function(Income){
  Dividers=seq(from = 0, to = 1, by = 0.25)
  Qs=quantile(Income, Dividers)
  
  Output=tapply(Income,findInterval(Income,Qs, rightmost.closed = TRUE),sum)
  names(Output)=paste(Dividers[1:4],"-",Dividers[2:5])
  return(Output/sum(Output))
}


ExtractElement = function(ElementName, ListOfAnalytics, SpecificTaxes=NULL ){
  n=length(ListOfAnalytics)
  IntList=vector("list", n)
  #VariableNames=vector("character", n)
    GetElement=function(TaxAnalysis) do.call("$",list(TaxAnalysis,ElementName))
  for(i in seq_len(n)) {
    IntList[[i]]=GetElement(ListOfAnalytics[[i]])
   # VariableNames[i]=ListOfAnalytics[[i]]$TaxSystemName
  }
  OutputList=sapply(X=IntList,FUN=unlist)
  #if(length(dim((OutputList))) == 2) colnames(OutputList)=(VariableNames)
  #if(length(dim(OutputList)) == 1) names(OutputList)=(VariableNames)
  if(is.vector(OutputList)&&!is.null(names(ListOfAnalytics))) names(OutputList)=names(ListOfAnalytics)
  if(is.matrix(OutputList)&&!is.null(names(ListOfAnalytics))) colnames(OutputList)=names(ListOfAnalytics)

  if(!is.null(SpecificTaxes)) OutputList=OutputList[SpecificTaxes,]
  OutputList
}


GraphsAndTables= function(AnalyticsExisting, AnalyticsNew) {
  ExistingSystemSummary = c(AnalyticsExisting$GrossIncome,  AnalyticsExisting$RevenueRaised,  AnalyticsExisting$PostAllTaxAndBenIncome)
  NewSystemSummary = c(AnalyticsNew$GrossIncome,  AnalyticsNew$RevenueRaised, AnalyticsNew$PostAllTaxAndBenIncome)
  Summary=data.frame(ExistingSystemSummary=ExistingSystemSummary,NewSystemSummary=NewSystemSummary)
  colnames(Summary) <- c("Existing Tax System", "New Tax System")
  rownames(Summary) <- c("Gross Income", "Total net tax bill","Income after all taxes and benefits")

  RevenueByTaxSummary <- round(data.frame(AnalyticsExisting$RevenueRaisedByTax,AnalyticsNew$RevenueRaisedByTax),2)
  colnames(RevenueByTaxSummary) <- c("Existing Tax System", "New Tax System")
  
  cols <- rainbow(length(RevenueByTaxSummary[,1]))
  par(mar=c(10,4,5,4)+.1, srt=90)
  barplot1 <- barplot(RevenueByTaxSummary[,1],width=2, beside=FALSE,main="Existing Tax System Breakdown",names.arg=row.names(RevenueByTaxSummary),ylab = "Tax Paid (£)",col=cols,las=2,ylim = c(0,max(RevenueByTaxSummary[,1])+5000))
  text(x=barplot1,y=as.numeric(RevenueByTaxSummary[,1]),label =as.numeric(RevenueByTaxSummary[,1]) ,pos=4,cex=1.1,col="black",las=1)
  
  barplot2 <- barplot(RevenueByTaxSummary[,2], width = 2, beside=FALSE,names.arg=row.names(RevenueByTaxSummary),main="New Tax System Breakdown", ylab = "Tax Paid (£)", col=cols,las=2,ylim=c(0,max(RevenueByTaxSummary[,2])+5000))
  text(x=barplot2,y=as.numeric(RevenueByTaxSummary[,2]),label =as.numeric(RevenueByTaxSummary[,2]),pos=4,cex=1.1,col="black",las=1)
  kable((round(Summary,0)), caption = paste0("Effect of tax systems"))
}






waterfall <- function(df, offset=0.3) {
  
  library(ggplot2)
  library(scales)
  library(dplyr)
  
  ## Add the order column to the raw data frame and order appropriately
  df <- df %>% mutate(order=as.numeric(category)) %>% arrange(order)
  
  ## The last value needs to be negated so that it goes down to
  ## zero.  Throws a warning if the cumulative sum doesn't match.
  last.id <- nrow(df)
  df$value[last.id] <- -df$value[last.id]
  
  ## Calculate the cumulative sums
  df <- df %>% mutate(cs1=cumsum(value))
  
  ## Throw a warning if the values don't match zero as expected
  final_value <- tail(df$cs1, 1)
  if (final_value!=0) {
    if(abs(final_value)<=2) {
      df$value[last.id] <- -sum(df$value[-last.id]) 
      } else {
          warning(sprintf("Final value doesn't return to 0.  %2d instead.", final_value))
        }
  }
  
  ## Calculate the max and mins for each category and sector
  df <- transform(df, min.val=c(0, head(cs1, -1)),
                  max.val=c(head(cs1, -1), 0))    
  df <- df %>% group_by(order, category, sector, value, cs1) %>%
    summarize(min=min(min.val, max.val), max=max(min.val, max.val))
  
  ## Create the lines data frame to link the bars
  lines <- df %>% group_by(order) %>% summarize(cs=max(cs1))
  lines <- with(lines, data.frame(x=head(order, -1),
                                  xend=tail(order, -1),
                                  y=head(cs, -1),
                                  yend=head(cs, -1)))
  
  
  ## Add the offset parameter
  df <- transform(df, offset=offset)
  
  ## Make the plot    
  gg <- ggplot() +
    geom_segment(data=lines, aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed")  +
    geom_rect(data=df, aes(xmin=order - offset,
                           xmax=order + offset, 
                           ymin=min,
                           ymax=max, fill=sector)) +
    scale_x_continuous(breaks=unique(df$order), labels=unique(df$category)
                       ) + theme(legend.position="none") +
    labs(x = "Quantity", y = "Value")
  
  
  return(gg)
}


aggregateRevenue = function(Population,TaxSystem,aggLookup=aggregationLookup,aggLevels=c("Cat2","Cat3"),scale=1,IncludeBeforeAndAfter=TRUE,TaxesNegative=TRUE){
  TRR=TotalRevenueRaised(Population,TaxSystem,IncludeBeforeAndAfter=IncludeBeforeAndAfter,TaxesNegative=TaxesNegative)
  TRR=data.frame(Type=names(TRR),Revenue=TRR)
  TRR=merge(TRR,aggLookup)
  TRR=aggregate(TRR$Revenue,(TRR[,aggLevels]),FUN=sum)
  TRR=TRR[,c(1,3,2)]
  colnames(TRR)=c("category","value","sector")
  TRR$value=TRR$value*scale
  TRR
}
