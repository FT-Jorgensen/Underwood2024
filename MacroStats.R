#Load in required packages (I like to use sqldf for database query as I am used to high-level languages like GIS interface)
install.packages("sqldf")
install.packages("vegan")
install.packages("ggpubr")
library(tidyverse)
library(ggplot2)
library(sqldf)
library(vegan)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
MacrosDraft<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/DATA - Macros.csv")

#Add in a "1" value in a new column for every individual macro- this is crucial for creating tables the right way
MacrosDraft$GraphCount<-rep(1,times=856)

#Playing around with practice bargraphs
#Total macros as a function of site
ggplot(data=MacrosDraft,mapping=aes(x=Site,y=GraphCount,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
#Macro biomass as a function of site
ggplot(data=MacrosDraft,mapping=aes(x=Site,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
#Total macros as a function of habitat
ggplot(data=MacrosDraft,mapping=aes(x=Habitat,y=GraphCount,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
#Macro biomass as a function of habitat
ggplot(data=MacrosDraft,mapping=aes(x=Habitat,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
#Macro biomass as a function of restoration or lack thereof
ggplot(data=MacrosDraft,mapping=aes(x=Restored,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()

#Parse out and calculate the total pervious, vegetated, and tree-covered land percentage for each site's watershed
J3LandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - J3.csv")
JMainLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - JabezMain.csv")
HBLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - HowardsBranch.csv")
ARLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - ArthursRun.csv")
CCLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - CattailCreek.csv")
CULandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - CattailCreekUpper.csv")
J3PerviousPct<-sum(sqldf("select Pct from J3LandUse where Porosity = 'Yes'"))
JMainPerviousPct<-sum(sqldf("SELECT Pct FROM JMainLandUse WHERE Porosity = 'Yes'"))
HBPerviousPct<-sum(sqldf("SELECT Pct FROM HBLandUse WHERE Porosity = 'Yes'"))
ARPerviousPct<-sum(sqldf("SELECT Pct FROM ARLandUse WHERE Porosity = 'Yes'"))
CCPerviousPct<-sum(sqldf("SELECT Pct FROM CCLandUse WHERE Porosity = 'Yes'"))
CUPerviousPct<-sum(sqldf("SELECT Pct FROM CULandUse WHERE Porosity = 'Yes'"))
J3VegPct<-sum(sqldf("SELECT Pct FROM J3LandUse WHERE Vegetated = 'Yes'"))
JMainVegPct<-sum(sqldf("SELECT Pct FROM JMainLandUse WHERE Vegetated = 'Yes'"))
HBVegPct<-sum(sqldf("SELECT Pct FROM HBLandUse WHERE Vegetated = 'Yes'"))
ARVegPct<-sum(sqldf("SELECT Pct FROM ARLandUse WHERE Vegetated = 'Yes'"))
CCVegPct<-sum(sqldf("SELECT Pct FROM CCLandUse WHERE Vegetated = 'Yes'"))
CUVegPct<-sum(sqldf("SELECT Pct FROM CULandUse WHERE Vegetated = 'Yes'"))
J3TreePct<-sum(sqldf("SELECT Pct FROM J3LandUse WHERE TreeCvr = 'Yes'"))
JMainTreePct<-sum(sqldf("SELECT Pct FROM JMainLandUse WHERE TreeCvr = 'Yes'"))
HBTreePct<-sum(sqldf("SELECT Pct FROM HBLandUse WHERE TreeCvr = 'Yes'"))
ARTreePct<-sum(sqldf("SELECT Pct FROM ARLandUse WHERE TreeCvr = 'Yes'"))
CCTreePct<-sum(sqldf("SELECT Pct FROM CCLandUse WHERE TreeCvr = 'Yes'"))
CUTreePct<-sum(sqldf("SELECT Pct FROM CULandUse WHERE TreeCvr = 'Yes'"))

#Prepare and create a new table/dataframe able to cross-compare crucial macro, land use, etc. information by site
SiteName<-c('Jabez 3','Jabez Main','Howards Branch','Arthurs Run','Cattail Creek Lower','Cattail Creek Upper')
PerviousPct<-c(J3PerviousPct,JMainPerviousPct,HBPerviousPct,ARPerviousPct,CCPerviousPct,CUPerviousPct)
VegPct<-c(J3VegPct,JMainVegPct,HBVegPct,ARVegPct,CCVegPct,CUVegPct)
TreePct<-c(J3TreePct,JMainTreePct,HBTreePct,ARTreePct,CCTreePct,CUTreePct)
Restored<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
FinalDataFrame<-data.frame(SiteName,PerviousPct,VegPct,TreePct,Restored)

#Calculate and add in the macroinvertebrate-specific benchmarks
FinalDataFrame$MacroTally<-c(sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU'")))
FinalDataFrame$TotalMass<-c(sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'J3'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'J1'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'HB'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'AR'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'CC'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'CU'")))
FinalDataFrame$EPTAsPct<-c((sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND TaxOrder = 'Ephemeroptera' OR Site = 'J1' AND TaxOrder = 'Plecoptera' OR Site = 'J1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND TaxOrder = 'Ephemeroptera' OR Site = 'HB' AND TaxOrder = 'Plecoptera' OR Site = 'HB' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND TaxOrder = 'Ephemeroptera' OR Site = 'AR' AND TaxOrder = 'Plecoptera' OR Site = 'AR' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND TaxOrder = 'Ephemeroptera' OR Site = 'CC' AND TaxOrder = 'Plecoptera' OR Site = 'CC' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND TaxOrder = 'Ephemeroptera' OR Site = 'CU' AND TaxOrder = 'Plecoptera' OR Site = 'CU' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU'")))*100)

# BIODIVERSITY INDICES
# Counting total number of individual taxa for each site
TaxaCount <- MacrosDraft %>% 
  pivot_longer(Family) %>% 
  count(name, value)
print(TaxaCount)
data.frame(TaxaCount)
J3Macros <- data.frame(sqldf("select * from MacrosDraft where Site = 'J3'"))
J3TaxaCount <- J3Macros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(J3TaxaCount)
JMainMacros <- data.frame(sqldf("select * from MacrosDraft where Site = 'J1'"))
JMainTaxaCount <- JMainMacros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(JMainTaxaCount)
HBMacros <- data.frame(sqldf("select * from MacrosDraft where Site = 'HB'"))
HBTaxaCount <- HBMacros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(HBTaxaCount)
ARMacros <- data.frame(sqldf("select * from MacrosDraft where Site = 'AR'"))
ARTaxaCount <- ARMacros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(ARTaxaCount)
CCMacros <- data.frame(sqldf("select * from MacrosDraft where Site = 'CC'"))
CCTaxaCount <- CCMacros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CCTaxaCount)
CUMacros <- data.frame(sqldf("select * from MacrosDraft where Site = 'CU'"))
CUTaxaCount <- CUMacros %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CUTaxaCount)
#Running Shannon's Diversity Index on each site (for individual species)
J3Shannon<- -sum((J3TaxaCount$n/sum(J3TaxaCount$n))*log((J3TaxaCount$n/sum(J3TaxaCount$n))))
JMainShannon<- -sum((JMainTaxaCount$n/sum(JMainTaxaCount$n))*log((JMainTaxaCount$n/sum(JMainTaxaCount$n))))
HBShannon<- -sum((HBTaxaCount$n/sum(HBTaxaCount$n))*log((HBTaxaCount$n/sum(HBTaxaCount$n))))
ARShannon<- -sum((ARTaxaCount$n/sum(ARTaxaCount$n))*log((ARTaxaCount$n/sum(ARTaxaCount$n))))
CCShannon<- -sum((CCTaxaCount$n/sum(CCTaxaCount$n))*log((CCTaxaCount$n/sum(CCTaxaCount$n))))
CUShannon<- -sum((CUTaxaCount$n/sum(CUTaxaCount$n))*log((CUTaxaCount$n/sum(CUTaxaCount$n))))
#Adding it back to original dataframe
FinalDataFrame$NumTaxa<-c(count(J3TaxaCount),count(JMainTaxaCount),count(HBTaxaCount),count(ARTaxaCount),count(CCTaxaCount),count(CUTaxaCount))
FinalDataFrame$SpeciesShannonDiversity <- c(J3Shannon,JMainShannon,HBShannon,ARShannon,CCShannon,CUShannon)

#Repeating the process for diversity of functional feeding group
FFGCount <- MacrosDraft %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
print(FFGCount)
data.frame(FFGCount)
J3FFGCount <- J3Macros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(J3FFGCount)
JMainFFGCount <- JMainMacros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(JMainFFGCount)
HBFFGCount <- HBMacros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(HBFFGCount)
ARFFGCount <- ARMacros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(ARFFGCount)
CCFFGCount <- CCMacros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(CCFFGCount)
CUFFGCount <- CUMacros %>% 
  pivot_longer(FFG) %>% 
  count(name, value)
data.frame(CUFFGCount)
J3FFGShannon<- -sum((J3FFGCount$n/sum(J3FFGCount$n))*log((J3FFGCount$n/sum(J3FFGCount$n))))
JMainFFGShannon<- -sum((JMainFFGCount$n/sum(JMainFFGCount$n))*log((JMainFFGCount$n/sum(JMainFFGCount$n))))
HBFFGShannon<- -sum((HBFFGCount$n/sum(HBFFGCount$n))*log((HBFFGCount$n/sum(HBFFGCount$n))))
ARFFGShannon<- -sum((ARFFGCount$n/sum(ARFFGCount$n))*log((ARFFGCount$n/sum(ARFFGCount$n))))
CCFFGShannon<- -sum((CCFFGCount$n/sum(CCFFGCount$n))*log((CCFFGCount$n/sum(CCFFGCount$n))))
CUFFGShannon<- -sum((CUFFGCount$n/sum(CUFFGCount$n))*log((CUFFGCount$n/sum(CUFFGCount$n))))
FinalDataFrame$NumFFG<-c(count(J3FFGCount),count(JMainFFGCount),count(HBFFGCount),count(ARFFGCount),count(CCFFGCount),count(CUFFGCount))
FinalDataFrame$FFGShannonDiversity <- c(J3FFGShannon,JMainFFGShannon,HBFFGShannon,ARFFGShannon,CCFFGShannon,CUFFGShannon)

#Updated 08-05-2024: Add in flow rate data for each site, collected by Troy and I on Troy's work days
FinalDataFrame$FlowSpeed <- c(0.00894,0.07565,0.00533,0.00294,NA,0.02798)

#STATISTICAL TESTS- ANOVA is used for comparing quantitative to one or more qualitative values
#Spearman rank correlation is used for 
FFGVsRestoration <- aov(FFGShannonDiversity~Restored,data=FinalDataFrame)
summary(FFGVsRestoration)
FFGVsRestoration2 <- aov(FFGShannonDiversity~Restored+TreePct,data=FinalDataFrame)
summary(FFGVsRestoration2)

cor.test(x=FinalDataFrame$TreePct,y=FinalDataFrame$MacroTally,method='spearman')
cor.test(x=FinalDataFrame$VegPct,y=FinalDataFrame$MacroTally,method='spearman')
cor.test(x=FinalDataFrame$PerviousPct,y=FinalDataFrame$MacroTally,method='spearman')
cor.test(x=FinalDataFrame$FlowSpeed,y=FinalDataFrame$MacroTally,method='spearman')

cor.test(x=FinalDataFrame$TreePct,y=FinalDataFrame$FFGShannonDiversity,method='spearman')
cor.test(x=FinalDataFrame$VegPct,y=FinalDataFrame$FFGShannonDiversity,method='spearman')
cor.test(x=FinalDataFrame$PerviousPct,y=FinalDataFrame$FFGShannonDiversity,method='spearman')
cor.test(x=FinalDataFrame$FlowSpeed,y=FinalDataFrame$FFGShannonDiversity,method='spearman')

cor.test(x=FinalDataFrame$TreePct,y=FinalDataFrame$EPTAsPct,method='spearman')
cor.test(x=FinalDataFrame$VegPct,y=FinalDataFrame$EPTAsPct,method='spearman')
cor.test(x=FinalDataFrame$PerviousPct,y=FinalDataFrame$EPTAsPct,method='spearman')
cor.test(x=FinalDataFrame$FlowSpeed,y=FinalDataFrame$EPTAsPct,method='spearman')

