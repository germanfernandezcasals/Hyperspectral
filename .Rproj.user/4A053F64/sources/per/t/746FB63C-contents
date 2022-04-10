## Header ####
## Creator:     German Agustin Fernandez Casals
## Main:        Hyperspectral data analysis
## Version:     2.1
## Creation:    2022-II-25
## Last edited: 2022-IV-06


## Index:                                       ####
#       1) Packages loading

#       2) Data import and transformation

#       3) Data visualization
#       3.1 Raster visualization
#       3.1.1 Single layer raster
#       3.1.2 RGB raster
#       3.1.3 SIPI (Structure Intensive Pigment Index)
#       3.1.4 CARI (Chlorophyll Absorption in Reflectance Index)
#       3.1.5 PSSRa-c (Pigment- Specific Simple Ratio)
#       3.2 Wavelength plots
#       3.2.1 Plots from the same data base
#       3.2.2 Plots from different data bases (for comparative)
#       3.1.3 Image
#       3.2 Using S3 class data (data frame)
#       3.2.1 Basic graph

#       4) Bounding boxes
#       4.1 Manual boundary box on the stem
#       4.2 Using CARI
#       4.2.1 Auto-detecting stem with an un-regular bounding boxes


## 1) Packages loading ####
if(!require("hyperSpec")) install.packages("hyperSpec")
if(!require("raster")) install.packages("raster")
if(!require("tictoc")) install.packages("tictoc")


## 2) Data import and transformation ####

# Notes:
# The names of the files are the first letter of the variety of the potato
# plant, follow of the plant number, the letter D of day, and 1 or 2 depending
# the day of the photograph session. For example G1D1 is Georgina plant 1 day 1.
# The importation of the data files is made as an hyperSpec (S4 class) 
# format as are .dat and .hdr files, and then are transformed to an S4 class
# data frame to work with, adding a "df" at the end of the name (ex. G1D1df).
# Once that transformation is done the original hyperSpec data is deleted from
# the R environment to salve RAM.


G1D1 <- read.ENVI(
  file = "../Data/2022-01-18 CERC potatoes/20220118_135248_G1/20220118_135248_G1.dat",
  headerfile = "../Data/2022-01-18 CERC potatoes/20220118_135248_G1/20220118_135248_G1.hdr",
  header = list(),
  keys.hdr2data = FALSE,
  wavelength = NULL,
  label = list(),
  block.lines.skip = 0,
  block.lines.size = NULL,
  pull.header.lines = TRUE)

G1D1df <- as.data.frame(G1D1, xy = TRUE)

rm(G1D1)

G1D2 <- read.ENVI(
  file = "../Data/2022-01-27 CERC potatoes/20220127_114743_G1/20220127_114743_G1.dat",
  headerfile = "../Data/2022-01-27 CERC potatoes/20220127_114743_G1/20220127_114743_G1.hdr",
  header = list(),
  keys.hdr2data = FALSE,
  wavelength = NULL,
  label = list(),
  block.lines.skip = 0,
  block.lines.size = NULL,
  pull.header.lines = TRUE)

G1D2df <- as.data.frame(G1D2, xy = TRUE)

rm (G1D2)

## 3) Data visualization ####

## 3.1 Raster visualization

# Notes:
# Different types of data visualization, not everyone will serve but it's nice to try


## 3.1.1 Single layer raster
ggplot() +
  geom_raster(data = G1D1df,
              aes (x = x, y = y, fill = spc[,32]))


## 3.1.2 RGB raster
rgb <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,114])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,65])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,32])))

plotRGB(rgb,
        r = 1,
        g = 2,
        b = 3,
        stretch = "lin")


## 3.1.3 SIPI (Structure Intensive Pigment Index)
sipi <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,199])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,22])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,139])))

pal <- colorRampPalette(c("black","green"))

plot((sipi[[1]] - sipi[[2]])/(sipi[[1]] + sipi[[3]]),
     axes = F,
     col = pal(50),
     main="Structure Intensive Pigment Index",
     sub="Georgina plant 1, day 1")

#or

ggplot() +
  geom_raster(data = G1D1df,
              aes (x = x, y = y,
                   fill = ((spc[,199] - spc[,22])/(spc[,199] + spc[,139])))) +
  scale_fill_gradientn(colours=c("black","green")) +
  labs(fill="SIPI Value",
       title = "Structure Intensive Pigment Index",
       subtitle = "Georgina plant 1, day 1",
       x = "", y = "") + 
  theme_minimal()


## 3.1.4 CARI (Chlorophyll Absorption in Reflectance Index)
cari <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,149])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,134])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,74])))

pal <- colorRampPalette(c("black","green"))

plot((cari[[1]] - cari[[2]]) - 0.2*(cari[[1]] - cari[[3]]),
     col = pal(50))

#or

ggplot() +
  geom_raster(data = G1D1df,
              aes (x = x, y = y,
                   fill = ((spc[,149] - spc[,134]) - 
                             0.2*(spc[,149] - spc[,74])))) +
  scale_fill_gradientn(colours=c("black","green")) +
  labs(fill="CARI Value",
       title = "Chlorophyll Absorption in Reflectance Index",
       subtitle = "Georgina plant 1, day 1",
       x = "", y = "") + 
  theme_minimal()


## 3.1.5 PSSRa-c (Pigment- Specific Simple Ratio)
PSSRac <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,199]/G1D1df$spc[,139])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,199]/G1D1df$spc[,117])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,199]/G1D1df$spc[,34])))

plotRGB(PSSRac,
        r = 1,
        g = 2,
        b = 3,
        stretch = "lin")


## 3.2 Wavelength plots

# Notes:
# Different types of data plots, was made in early stages and probably 
# will be useful in a future


## 3.2.1 Plots from the same data base

#All wavelength plot
qplotspc(G1D1) #qplotspc is because is a class S4 database

#Plot of all wavelength and a zone
qplotspc(G1D1, c(550 ~ 750, min ~ 1200))

#Mean and quantile plot, I couldn't put title to the plot or axis
plot(G1D1, col = "red", y = "spcprctile") 
abline (v = c(402, 750), col = c("black", "darkgreen")) #Test: visible spectrum

#Standard deviation 
plot(G1D1, func = sd) 

#SD and mean with ggplot2
qplotspc(mean(G1D1)) +  
  geom_ribbon (aes (ymin = mean + sd,
                    ymax = mean - sd,
                    y = 0, group = NA),
               alpha = 0.25,
               data = as.t.df (mean_sd (G1D1)))

#RGB wavelength
plot(G1D1[c(630, 532, 465)], col = c("red", "green", "blue"))

#Plot of a particular wavelength range
plot(G1D1[,, 700 ~ 1200], col = matlab.dark.palette (6))

#Histogram
ggplot(G1D1df, aes(x = spc[,"630"])) +
  geom_histogram()

#qqplot (without the qqline)
qqplot(G1D1df$spc[,"630"], G1D1df$spc[,"532"]) # take some minutes / don't work with qqnorm

#boxplot
boxplot(G1D1df$spc[,"630"], col = "red")


## 3.2.2 Plots from different data bases (for comparation) 
plot (G1D1[630], col = "black")
plot (G1D2[630], col = "red", add = TRUE) 

plot(G1D1, col = "black", y = "spcprctile")
plot(G1D2, col = "yellow", y = "spcprctile", add = TRUE)
abline (v = c(402, 750), col = c("black", "darkgreen"))


## 4) Bounding boxes ####

## 4.1  Manual boundary box on the stem

# Notes:
# The coordinates of the boundary box are calculated manually
# with the size of 100 x 100 pixels


##G1D1
rgb <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,114])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,65])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,32])))

bbox1 <- spPolygons(rbind(c(400,200), c(400,300), c(500, 300), c(500,200)))

plotRGB(rgb,
        r = 1,
        g = 2,
        b = 3,
        stretch = "lin", 
        addfun = lines(bbox1))


##G1D2
rgb <- brick(
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,114])),
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,65])),
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,32])))

bbox1 <- spPolygons(rbind(c(350,250), c(350,350), c(450, 350), c(450,250)))

plotRGB(rgb,
        r = 1,
        g = 2,
        b = 3,
        stretch = "lin", 
        addfun = lines(bbox1))


## 4.2 Using CARI

## 4.2.1 Auto-detecting stem with an un-regular bounding boxes

# Notes:
# This system have the flow that is not very precise, mainly on the G1D2,
# also only work with plants that have only one stem.
# Be careful with the RAM if you want to plot the two rasters at the same time


##G1D1
cari <- brick(
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,149])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,134])),
  rasterFromXYZ(data.frame(G1D1df$x, G1D1df$y, G1D1df$spc[,74])))

.bb1 <- ((cari[[1]] - cari[[2]]) - 0.2*(cari[[1]] - cari[[3]]) >= 3.5)
.bb2 <- as.data.frame(.bb1, xy = T)
.bb3 <- subset(.bb2, .bb2$layer == "TRUE")

bbox1 <- spPolygons(extent(.bb3), crs = crs(.bb1))
bbox(bbox1)

pal <- colorRampPalette(c("black","green"))

plot((cari[[1]] - cari[[2]]) - 0.2*(cari[[1]] - cari[[3]]),
     col = pal(50)) + 
  lines(bbox1)


##G1D2
cari <- brick(
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,149])),
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,134])),
  rasterFromXYZ(data.frame(G1D2df$x, G1D2df$y, G1D2df$spc[,74])))

.bb1 <- ((cari[[1]] - cari[[2]]) - 0.2*(cari[[1]] - cari[[3]]) >= 3.75)
.bb2 <- as.data.frame(.bb1, xy = T)
.bb3 <- subset(.bb2, .bb2$layer == "TRUE")
plot(.bb1)
bbox1 <- spPolygons(extent(.bb3), crs = crs(.bb1))
bbox(bbox1)

pal <- colorRampPalette(c("black","green"))

plot((cari[[1]] - cari[[2]]) - 0.2*(cari[[1]] - cari[[3]]),
     col = pal(50)) + 
  lines(bbox1)