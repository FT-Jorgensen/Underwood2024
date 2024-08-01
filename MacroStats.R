#Load in required packages (I like to use RSQLite for database query as I am used to high-level languages like GIS interface)
library(ggplot2)
library(RSQLite)
MacrosDraft<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/DATA - Macros.csv")

#Playing around with practice bar
ggplot(data=MacrosDraft,mapping=aes(x=Site,y=GraphCount,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
MacrosDraft$GraphCount<-rep(1,times=601)
help(geom_bar)

ggplot(data=MacrosDraft,mapping=aes(x=Site,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
ggplot(data=MacrosDraft,mapping=aes(x=Habitat,y=GraphCount,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
ggplot(data=MacrosDraft,mapping=aes(x=Habitat,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()
ggplot(data=MacrosDraft,mapping=aes(x=Restored,y=Weight,fill=Family)) + geom_bar(stat="identity",na.rm=TRUE) + scale_fill_hue()

J3LandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - J3.csv")
JMainLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - JabezMain.csv")
HBLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - HowardsBranch.csv")
ARLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - ArthursRun.csv")
CCLandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - CattailCreek.csv")
CULandUse<-read.csv("/Users/tjorgensen/Desktop/Underwood 2024/LandUseTables.xlsx - CattailCreekUpper.csv")
J3PerviousPct<-sum(sqldf("SELECT Pct FROM J3LandUse WHERE Porosity = 'Yes'"))
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

SiteName<-c('Jabez 3','Jabez Main','Howards Branch','Arthurs Run','Cattail Creek Lower','Cattail Creek Upper')
PerviousPct<-c(J3PerviousPct,JMainPerviousPct,HBPerviousPct,ARPerviousPct,CCPerviousPct,CUPerviousPct)
VegPct<-c(J3VegPct,JMainVegPct,HBVegPct,ARVegPct,CCVegPct,CUVegPct)
TreePct<-c(J3TreePct,JMainTreePct,HBTreePct,ARTreePct,CCTreePct,CUTreePct)
LandUsePcts<-data.frame
Restored<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
LandUsePct<-data.frame(SiteName,PerviousPct,VegPct,TreePct,Restored)
LandUsePct$MacroTally<-c(sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC'")),sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU'")))
LandUsePct$TotalMass<-c(sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'J3'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'J1'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'HB'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'AR'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'CC'")),sum(sqldf("SELECT Weight FROM MacrosDraft WHERE Site = 'CU'")))
LandUsePct$EPTAsPct<-c((sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1' AND TaxOrder = 'Ephemeroptera' OR Site = 'J1' AND TaxOrder = 'Plecoptera' OR Site = 'J1' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB' AND TaxOrder = 'Ephemeroptera' OR Site = 'HB' AND TaxOrder = 'Plecoptera' OR Site = 'HB' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'HB'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR' AND TaxOrder = 'Ephemeroptera' OR Site = 'AR' AND TaxOrder = 'Plecoptera' OR Site = 'AR' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'AR'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC' AND TaxOrder = 'Ephemeroptera' OR Site = 'CC' AND TaxOrder = 'Plecoptera' OR Site = 'CC' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CC'")))*100,
            (sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU' AND TaxOrder = 'Ephemeroptera' OR Site = 'CU' AND TaxOrder = 'Plecoptera' OR Site = 'CU' AND TaxOrder = 'Trichoptera'"))/sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'CU'")))*100)
            


(sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J3' AND TaxOrder = 'Ephemeroptera' OR Site = 'J3' AND TaxOrder = 'Plecoptera' OR Site = 'J3' AND TaxOrder = 'Trichoptera'")))
sum(sqldf("SELECT GraphCount FROM MacrosDraft WHERE Site = 'J1'"))


