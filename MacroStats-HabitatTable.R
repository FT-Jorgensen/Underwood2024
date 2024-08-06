#This script creates a similar "FinalDataFrame" but instead groups attributes by individual sampling location as opposed to whole site
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
library(wesanderson)

MacrosDraft<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/DATA - Macros.csv")
#Add in a "1" value in a new column for every individual macro- this is crucial for creating tables the right way
MacrosDraft$GraphCount<-rep(1,times=856)

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

#Parse out the eighteen individual sampling sites
J31Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '1'"))
J32Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '2'"))
J33Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '3'"))
JMain1Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '1'"))
JMain2Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '2'"))
JMain3Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '3'"))
HB1Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '1'"))
HB2Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '2'"))
HB3Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '3'"))
AR1Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '1'"))
AR2Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '2'"))
AR3Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '3'"))
CC1Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '1'"))
CC2Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '2'"))
CC3Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '3'"))
CU1Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '1'"))
CU2Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '2'"))
CU3Tbl<-data.frame(sqldf("SELECT * FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '3'"))

#Prepare the table
SiteName<-c('Jabez 3.1','Jabez 3.2','Jabez 3.3',
            'Jabez Main.1','Jabez Main.2','Jabez Main.3',
            'Howards Branch.1','Howards Branch.2','Howards Branch.3',
            'Arthurs Run.1','Arthurs Run.2','Arthurs Run.3',
            'Cattail Creek Lower.1','Cattail Creek Lower.2','Cattail Creek Lower.3',
            'Cattail Creek Upper.1','Cattail Creek Upper.2','Cattail Creek Upper.3')
PerviousPct<-c(rep(J3PerviousPct,times=3),rep(JMainPerviousPct,times=3),rep(HBPerviousPct,times=3),
               rep(ARPerviousPct,times=3),rep(CCPerviousPct,times=3),rep(CUPerviousPct,times=3))
VegPct<-c(rep(J3VegPct,times=3),rep(JMainVegPct,times=3),rep(HBVegPct,times=3),
               rep(ARVegPct,times=3),rep(CCVegPct,times=3),rep(CUVegPct,times=3))
TreePct<-c(rep(J3TreePct,times=3),rep(JMainTreePct,times=3),rep(HBTreePct,times=3),
               rep(ARTreePct,times=3),rep(CCTreePct,times=3),rep(CUTreePct,times=3))
Restored<-c(rep(TRUE,times=3),rep(FALSE,times=3),rep(TRUE,times=3),
           rep(FALSE,times=3),rep(TRUE,times=3),rep(FALSE,times=3))
Size<-c(rep(3032553,times=3),rep(10389250,times=3),rep(948410,times=3),
        rep(428450,times=3),rep(6373450,times=3),rep(4471850,times=3))
Habitat<-c("R","V","R","U","R","U","R","V","R","L","L","L","R","R","V","U","U","V")
FlowRate<-c(rep(0.00894,times=3),rep(0.07565,times=3),rep(0.00533,times=3),
            rep(0.00294,times=3),rep(0.04446,times=3),rep(0.02798,times=3))
MacroTally<-c(sum(J31Tbl$GraphCount),sum(J32Tbl$GraphCount),sum(J33Tbl$GraphCount),
             sum(JMain1Tbl$GraphCount),sum(JMain2Tbl$GraphCount),sum(JMain3Tbl$GraphCount),
             sum(HB1Tbl$GraphCount),sum(HB2Tbl$GraphCount),sum(HB3Tbl$GraphCount),
             sum(AR1Tbl$GraphCount),sum(AR2Tbl$GraphCount),sum(AR3Tbl$GraphCount),
             sum(CC1Tbl$GraphCount),sum(CC2Tbl$GraphCount),sum(CC3Tbl$GraphCount),
             sum(CU1Tbl$GraphCount),sum(CU2Tbl$GraphCount),sum(CU3Tbl$GraphCount))
TotalMass<-c(sum(J31Tbl$Weight),sum(J3.2Tbl$Weight),sum(J3.3Tbl$Weight),
             sum(JMain1Tbl$Weight),sum(JMain.2Tbl$Weight),sum(JMain.3Tbl$Weight),
             sum(HB1Tbl$Weight),sum(HB2Tbl$Weight),sum(HB3Tbl$Weight),
             sum(AR1Tbl$Weight),sum(AR2Tbl$Weight),sum(AR3Tbl$Weight),
             sum(CC1Tbl$Weight),sum(CC2Tbl$Weight),sum(CC3Tbl$Weight),
             sum(CU1Tbl$Weight),sum(CU2Tbl$Weight),sum(CU3Tbl$Weight))
EPTAsPct<-c((sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND SiteNo = '3' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'J1' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'J1' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'J1' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'J1' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'J1' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'J1' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND SiteNo = '3' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'HB' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'HB' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'HB' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'HB' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'HB' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'HB' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND SiteNo = '3' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'AR' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'AR' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'AR' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'AR' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'AR' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'AR' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND SiteNo = '3' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'CC' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'CC' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'CC' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'CC' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'CC' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'CC' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND SiteNo = '3' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '1' AND TaxOrder = 'Ephemeroptera' OR Site = 'CU' AND SiteNo = '1' AND TaxOrder = 'Plecoptera' OR Site = 'CU' AND SiteNo = '1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '1' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '2' AND TaxOrder = 'Ephemeroptera' OR Site = 'CU' AND SiteNo = '2' AND TaxOrder = 'Plecoptera' OR Site = 'CU' AND SiteNo = '2' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '2' ")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '3' AND TaxOrder = 'Ephemeroptera' OR Site = 'CU' AND SiteNo = '3' AND TaxOrder = 'Plecoptera' OR Site = 'CU' AND SiteNo = '3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND SiteNo = '3' ")))*100)
FinalDataFrameHABITAT<-data.frame(SiteName,Restored,Size,Habitat,PerviousPct,VegPct,TreePct,FlowRate,MacroTally,TotalMass,EPTAsPct)
TaxaCount <- MacrosDraft %>% 
  pivot_longer(Family) %>% 
  count(name, value)
print(TaxaCount)
data.frame(TaxaCount)
J31TaxaCount <- J31Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(J31TaxaCount)
J32TaxaCount <- J32Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(J32TaxaCount)
J33TaxaCount <- J33Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(J33TaxaCount)
JMain1TaxaCount <- JMain1Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(JMain1TaxaCount)
JMain2TaxaCount <- JMain2Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(JMain2TaxaCount)
JMain3TaxaCount <- JMain3Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(JMain3TaxaCount)
HB1TaxaCount <- HB1Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(HB1TaxaCount)
HB2TaxaCount <- HB2Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(HB2TaxaCount)
HB3TaxaCount <- HB3Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(HB3TaxaCount)
AR1TaxaCount <- AR1Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(AR1TaxaCount)
AR2TaxaCount <- AR2Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(AR2TaxaCount)
AR3TaxaCount <- AR3Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(AR3TaxaCount)
CC1TaxaCount <- CC1Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CC1TaxaCount)
CC2TaxaCount <- CC2Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CC2TaxaCount)
CC3TaxaCount <- CC3Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CC3TaxaCount)
CU1TaxaCount <- CU1Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CU1TaxaCount)
CU2TaxaCount <- CU2Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CU2TaxaCount)
CU3TaxaCount <- CU3Tbl %>% 
  pivot_longer(Family) %>% 
  count(name, value)
data.frame(CU3TaxaCount)
J31Shannon<- -sum((J31TaxaCount$n/sum(J31TaxaCount$n))*log((J31TaxaCount$n/sum(J31TaxaCount$n))))
J32Shannon<- -sum((J32TaxaCount$n/sum(J32TaxaCount$n))*log((J32TaxaCount$n/sum(J32TaxaCount$n))))
J33Shannon<- -sum((J33TaxaCount$n/sum(J33TaxaCount$n))*log((J33TaxaCount$n/sum(J33TaxaCount$n))))
JMain1Shannon<- -sum((JMain1TaxaCount$n/sum(JMain1TaxaCount$n))*log((JMain1TaxaCount$n/sum(JMain1TaxaCount$n))))
JMain2Shannon<- -sum((JMain2TaxaCount$n/sum(JMain2TaxaCount$n))*log((JMain2TaxaCount$n/sum(JMain2TaxaCount$n))))
JMain3Shannon<- -sum((JMain3TaxaCount$n/sum(JMain3TaxaCount$n))*log((JMain3TaxaCount$n/sum(JMain3TaxaCount$n))))
HB1Shannon<- -sum((HB1TaxaCount$n/sum(HB1TaxaCount$n))*log((HB1TaxaCount$n/sum(HB1TaxaCount$n))))
HB2Shannon<- -sum((HB2TaxaCount$n/sum(HB2TaxaCount$n))*log((HB2TaxaCount$n/sum(HB2TaxaCount$n))))
HB3Shannon<- -sum((HB3TaxaCount$n/sum(HB3TaxaCount$n))*log((HB3TaxaCount$n/sum(HB3TaxaCount$n))))
AR1Shannon<- -sum((AR1TaxaCount$n/sum(AR1TaxaCount$n))*log((AR1TaxaCount$n/sum(AR1TaxaCount$n))))
AR2Shannon<- -sum((AR2TaxaCount$n/sum(AR2TaxaCount$n))*log((AR2TaxaCount$n/sum(AR2TaxaCount$n))))
AR3Shannon<- -sum((AR3TaxaCount$n/sum(AR3TaxaCount$n))*log((AR3TaxaCount$n/sum(AR3TaxaCount$n))))
CC1Shannon<- -sum((CC1TaxaCount$n/sum(CC1TaxaCount$n))*log((CC1TaxaCount$n/sum(CC1TaxaCount$n))))
CC2Shannon<- -sum((CC2TaxaCount$n/sum(CC2TaxaCount$n))*log((CC2TaxaCount$n/sum(CC2TaxaCount$n))))
CC3Shannon<- -sum((CC3TaxaCount$n/sum(CC3TaxaCount$n))*log((CC3TaxaCount$n/sum(CC3TaxaCount$n))))
CU1Shannon<- -sum((CU1TaxaCount$n/sum(CU1TaxaCount$n))*log((CU1TaxaCount$n/sum(CU1TaxaCount$n))))
CU2Shannon<- -sum((CU2TaxaCount$n/sum(CU2TaxaCount$n))*log((CU2TaxaCount$n/sum(CU2TaxaCount$n))))
CU3Shannon<- -sum((CU3TaxaCount$n/sum(CU3TaxaCount$n))*log((CU3TaxaCount$n/sum(CU3TaxaCount$n))))
FinalDataFrameHABITAT$NumTaxa<- c(count(J31TaxaCount),count(J32TaxaCount),count(J33TaxaCount),
                                  count(JMain1TaxaCount),count(JMain2TaxaCount),count(JMain3TaxaCount),
                                  count(HB1TaxaCount),count(HB2TaxaCount),count(HB3TaxaCount),
                                  count(AR1TaxaCount),count(AR2TaxaCount),count(AR3TaxaCount),
                                  count(CC1TaxaCount),count(CC2TaxaCount),count(CC3TaxaCount),
                                  count(CU1TaxaCount),count(CU2TaxaCount),count(CU3TaxaCount))


