TaxpayerIncomePreAndPostTax <- read.csv("data/TaxpayerIncomePreAndPostTax.csv",header = TRUE)
LogNormDetails=fitdistr(TaxpayerIncomePreAndPostTax$Total.income.before.tax, "lognormal")
HouseholdWealthDist <- read.csv("data/HouseholdWealthDistributionData.csv", header = TRUE)
Property <-HouseholdWealthDist$TotalNetPropertyWealthGBP[c(10,20,30,40,50,60,70,80,90,99)]
HouseholdConsTaxBase <- read.csv("data/HouseholdConsumptionTaxBase.csv",header = TRUE)
HouseholdDataLookup <- cbind(HouseholdConsTaxBase,Property)
HouseholdConsumptionTaxBaseData <- HouseholdConsTaxBase
HouseholdConsumptionTaxBaseLookup <- cbind(HouseholdConsumptionTaxBaseData,Property)
#VAT	Tobacco	Beer	Wine	Hydrocarbons	Customs	Betting	Insurance	Lottery	AirTravel	TvLicence	Carbon
#Assumed distributions match, lowest income have lowest wealth endowment

#c Set up population zero
SimPopulation = 42000
IncomeDist0 = PreTaxIncomeModelledFunction (LogNormDetails, n=SimPopulation)
Population0 = ConstructPopulationData(Income=IncomeDist0,HouseholdDataLookup)

ratioLogNormToPareto=dlnorm(75000,meanlog=10,sdlog=0.6)/dpareto(75000,scale=75000,shape=2)
ProportionZero=0.01
percentInPareto=(1-ProportionZero)*ratioLogNormToPareto
percentLogNorm=(1-ProportionZero)*(1-ratioLogNormToPareto)

nSimPopulationCalc = 4200
set.seed(1)
# LogNormDetails
# meanlog        sdlog   
# 10.09210356    0.56431483 
# ( 0.05671577) ( 0.04010411)
IncomeDistModelled1 <- rlnorm(round(1000*(percentLogNorm)), meanlog = 10,sdlog = 0.6)
#LogNormDetails$estimate["meanlog"],
                 #LogNormDetails$estimate["sdlog"])
IncomeDistModelled2=rpareto(round(1000*(percentInPareto)),scale=75000,shape=2)

IncomeDistModelled=c(IncomeDistModelled1,IncomeDistModelled2,rep(0, round(1000*ProportionZero)))

SortedIncome <- sort(IncomeDistModelled)
return(SortedIncome)

IncomeDistCalc = PreTaxIncomeModelledFunction (LogNormDetails, n=nSimPopulationCalc)
PopulationCalc = ConstructPopulationData(Income=IncomeDist0,HouseholdDataLookup)


#d Set up individual case studies
Individual1 = ConstructPopulationData(Income=20000, HouseholdDataLookup,Name="Fred",Pop=1,PropertyInput = 0)
Individual2 = ConstructPopulationData(Income=50000, HouseholdDataLookup,Name="Nick",Pop=1,PropertyInput = 357000)
Individual3 = ConstructPopulationData(Income=200000,HouseholdDataLookup,Name="Nick",Pop=1,PropertyInput = 1500000)
