
#Existing Tax System
ExistingIncTaxSystem <- CreateIncTaxSchedule(BandName=c("Pers all","Basic Rate","Higher Rate","Lose Pers All","Higher Rate","Top Rate"),Rate=c(0, 0.2, 0.4, 0.6, 0.4, 0.45), LowerLimit=c(0, 11500, 43000, 100000, 123000, 150000))
ExistingEmployeeNISystem <- CreateIncTaxSchedule(BandName=c("Pers All","Main Rate","Above Upper Limit"),Rate=c(0,0.12,0.02),LowerLimit=c(0,663*12,3489*12))
ExistingEmployerNISystem <- CreateIncTaxSchedule(BandName=c("Pers All","Main Rate","Above Upper Limit"),Rate=c(0,0.138,0.138),LowerLimit=c(0,663*12,3489*12))
ExistingIndirectTaxes <- list(VAT= 0.2,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 18,Property = 0.01)
ExistingBenefits <- list(CitDiv=0)
ExistingTaxSystem <- CreateFullTaxSystem(ExistingIncTaxSystem,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)

#No Systems
ZeroIndirectTaxes <- list(VAT = 0, Tobacco = 0, Beer = 0, Wine = 0, Hydrocarbons = 0, CustomsGoods = 0, Betting = 0, Insurance = 0, Lottery = 0, AirTravel = 0, TvLicence = 0, Carbon = 0, Property = 0)
ZeroNISystem <- CreateIncTaxSchedule(BandName=c("Pers All","Main Rate"),Rate=c(0,0),LowerLimit=c(0,10000))

#New Income Tax Systems

TaxSystem0Inc <- CreateFullTaxSystem(ExistingIncTaxSystem,ZeroNISystem,ZeroNISystem,ZeroIndirectTaxes,ExistingBenefits)
IncTaxSystem1 <- CreateIncTaxSchedule(BandName=c("Pers all","Basic Rate"),Rate=c(0, 0.4), LowerLimit=c(0, 0))
#TaxSystem1 <- CreateFullTaxSystem(IncTaxSystem1,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
TaxSystem1 <- CreateFullTaxSystem(IncTaxSystem1,ZeroNISystem,ZeroNISystem,ZeroIndirectTaxes,ExistingBenefits)
IncTaxSystem2 <- CreateIncTaxSchedule(BandName=c("Flat Rate"), Rate=0.3, LowerLimit=0)
#TaxSystem2 <- CreateFullTaxSystem(IncTaxSystem2,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
TaxSystem2 <- CreateFullTaxSystem(IncTaxSystem2,ZeroNISystem,ZeroNISystem,ZeroIndirectTaxes,ExistingBenefits)
IncTaxSystem3 <- CreateIncTaxSchedule(BandName=c("Basic Rate","Higher Rate"),Rate=c(0.25, 0.5), LowerLimit=c(0, 40000))
#TaxSystem3 <- CreateFullTaxSystem(IncTaxSystem3,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
TaxSystem3 <- CreateFullTaxSystem(IncTaxSystem3,ZeroNISystem,ZeroNISystem,ZeroIndirectTaxes, ExistingBenefits)

#New NI Systems
NewEmployeeNISystem <- CreateIncTaxSchedule(BandName=c("Pers All","Main Rate"),Rate=c(0,0.0),LowerLimit=c(0,12000))
NewEmployerNISystem <- CreateIncTaxSchedule(BandName=c("Pers All","Main Rate"),Rate=c(0,0.0),LowerLimit=c(0,12000))
TaxSystem0NI <- CreateFullTaxSystem(ExistingIncTaxSystem,NewEmployeeNISystem,NewEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)


#New Indirect Tax System

TaxSystemInd0=CreateFullTaxSystem(ExistingIncTaxSystem,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
NewIndirectTaxes1=list(VAT= 0.4,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 18,Property = 0.01)
TaxSystemInd1=CreateFullTaxSystem(IncTaxSystem1,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes1,ExistingBenefits)
NewIndirectTaxes2=list(VAT= 0.2,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 36,Property = 0.01)
TaxSystemInd2=CreateFullTaxSystem(IncTaxSystem1,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes2,ExistingBenefits)
NewIndirectTaxes3=list(VAT= 0.2,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 18,Property = 0.02)
TaxSystemInd3=CreateFullTaxSystem(IncTaxSystem1,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes3,ExistingBenefits)
NewIndirectTaxes4=list(VAT= 0.2,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 100,Property = 0.02)
TaxSystemInd4=CreateFullTaxSystem(IncTaxSystem1,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes4,ExistingBenefits)
#New proposals
NewIndirectTaxes5=list(VAT= 0.0,Tobacco= 5.61345/8.81,Beer= 0.8586/6.08875,Wine= 2.887/7.413,Hydrocarbons= 0.58/1.1695,CustomsGoods= 0.025,Betting= 0.15,Insurance= 0.1 ,Lottery= 0.12,AirTravel= 26,TvLicence= 147,Carbon= 200,Property = 0.01)
TaxSystemInd5=CreateFullTaxSystem(IncTaxSystem1,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes5,ExistingBenefits)



#New Full Tax System
ExistingTaxSystem <- CreateFullTaxSystem(ExistingIncTaxSystem,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
TaxSystemFull1=CreateFullTaxSystem(ExistingIncTaxSystem,ExistingEmployeeNISystem,ExistingEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
Benefits4K <- list(CitDiv=-4000)
Benefits6K <- list(CitDiv=-6000)
TaxSystemFull2=CreateFullTaxSystem(IncTaxSystem2,NewEmployeeNISystem,NewEmployerNISystem,ExistingIndirectTaxes,ExistingBenefits)
TaxSystemFull3=CreateFullTaxSystem(IncTaxSystem2,NewEmployeeNISystem,NewEmployerNISystem,ExistingIndirectTaxes,Benefits4K)
TaxSystemFull4=CreateFullTaxSystem(IncTaxSystem3,NewEmployeeNISystem,NewEmployerNISystem,NewIndirectTaxes4,Benefits6K)
TaxSystemFull5 = CreateFullTaxSystem(IncTaxSystem1,ZeroNISystem,ZeroNISystem,NewIndirectTaxes5,ExistingBenefits)
TaxSystemFull6 = CreateFullTaxSystem(IncTaxSystem1,ZeroNISystem,ZeroNISystem,NewIndirectTaxes5,Benefits4K)
