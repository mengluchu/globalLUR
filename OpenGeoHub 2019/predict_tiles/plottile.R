library(rasterVis)
library(raster)
library(RColorBrewer)
library(colorRamps)

dd= raster("E:/OpenGeoHub 2019/tiles/output/deLaday.tif")
dn= raster("E:/OpenGeoHub 2019/tiles/output/deLanight.tif")


dxd= raster("E:/OpenGeoHub 2019/tiles/output/dexgbday.tif")
dxn= raster("E:/OpenGeoHub 2019/tiles/output/dexgbnight.tif")

my.at <- seq(5, 45, by = 1)

#myTheme <- rasterTheme(region=c(brewer.pal(7, "Blues"),brewer.pal(7, "OrRd") ))
myTheme <- rasterTheme(region=c(brewer.pal(4, "Greys"), colorRamps::matlab.like2(n =10) ))
attrnames = c("day Lasso","night Lasso", "day xgb", "night xgb")
pdfname = "E:/OpenGeoHub 2019/tiles/output/esdn_M_LA.pdf"
s1 = stack(dd,dn,dxd,dxn)
pdf(pdfname)
levelplot(s1,names.attr=attrnames,at = my.at, par.settings = myTheme )
dev.off()
