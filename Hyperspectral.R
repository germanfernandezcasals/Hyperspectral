## Header ####

## Who: Germán Agustín Fernández Casals
## What: Dissertation sample data (test)
## Date: 2022-II-25
## Last edited: 2022-III-31

## 1) Packages loading and data management ####

if(!require("hyperSpec")) install.packages("hyperSpec")
if(!require("raster")) install.packages("raster")

## 2) Data import and transformation ####

## 2.1 Set directory

setwd(r'(D:\German Disk\OneDrive\Documentos\Harper Adams Uni\Dissertation\RStat_Dissertation)')

## 2.2 Data loading

G1D1 <- read.ENVI(
  file = "Data/2022-01-18 CERC potatoes/20220118_135248_G1/20220118_135248_G1.dat",
  headerfile = "Data/2022-01-18 CERC potatoes/20220118_135248_G1/20220118_135248_G1.hdr",
  header = list(),
  keys.hdr2data = FALSE,
  wavelength = NULL,
  label = list(),
  block.lines.skip = 0,
  block.lines.size = NULL,
  pull.header.lines = TRUE)

G1D2 <- read.ENVI(
  file = "Data/2022-01-27 CERC potatoes/20220127_114743_G1/20220127_114743_G1.dat",
  headerfile = "Data/2022-01-27 CERC potatoes/20220127_114743_G1/20220127_114743_G1.hdr",
  header = list(),
  keys.hdr2data = FALSE,
  wavelength = NULL,
  label = list(),
  block.lines.skip = 0,
  block.lines.size = NULL,
  pull.header.lines = TRUE)

## 2.3 Data S4 class test

class(G1D1)
chk.hy(G1D1)      #Validity Checking of hyperspect S4 class
validObject(G1D1) #Validity Checking of objects
str(G1D1)

class(G1D2)
chk.hy(G1D2)
validObject(G1D2)
str(G1D2)

## 2.4 Data transformation
G1D1df <- as.data.frame(G1D1)
G1D2df <- as.data.frame(G1D1)

## 2.3 Data S4 class test

class(G1D1df)
chk.hy(G1D1df)      
validObject(G1D1df) 
str(G1D1)

class(G1D2)
chk.hy(G1D2)      
validObject(G1D2)
str(G1D2)

## 3) Data visualization ####

## 3.1.1 Using S4 class data (hyperspect)

## 3.1.1 Wavelength plots
plot(G1D1, col = "red", y = "spcprctile") #Mean and quantile plot, I couldn't put title to the plot or axis 
abline (v = c(402, 750), col = c("black", "darkgreen")) #Test: visible spectrum

plot(G1D1, func = sd) #Standard deviation 

qplotspc(mean(G1D1)) +   #SD and mean with ggplot2 / qplotspc is because is a S4
  geom_ribbon (aes (ymin = mean + sd,
                    ymax = mean - sd,
                    y = 0, group = NA),
               alpha = 0.25,
               data = as.t.df (mean_sd (G1D1)))

plot(G1D1[c(630, 532, 465)], col = c("red", "green", "blue")) #RGB wavelength

plot(G1D1[,, 700 ~ 1200], col = matlab.dark.palette (6)) #plot particular wavelength range

qplotspc(G1D1)

qplotspc(G1D1, c(550 ~ 750, min ~ 1200))

## 3.1.2 Plots from different data
plot (G1D1[630], col = "black")
plot (G1D2[630], col = "red", add = TRUE) 

plot(G1D1, col = "black", y = "spcprctile")
plot(G1D2, col = "yellow", y = "spcprctile", add = TRUE)
abline (v = c(402, 750), col = c("black", "darkgreen"))


## 3.1.3 Image

plotmap(G1D1[,,630])

levelplot(spc ~ y * x | as.factor(.wavelength),   #RGB separate plot
          G1D1[,,c(630, 532, 465)],
          aspect = "iso",
          col.regions = matlab.palette(20)) #Change palette


## 3.2 Using S3 class data (data frame)

## 3.2.1 Basic graph

ggplot(G1D1df, aes(x = spc[,"630"])) +
  geom_histogram()

qqplot(G1D1df$spc[,"630"], G1D1df$spc[,"532"]) # take some minutes / don't work with qqnorm

boxplot(G1D1df$spc[,"630"], col = "red")

## 4) Analysis S4 ####

colnames(G1D1Dat)
G1D1dist <- pearson.dist (G1D1[[]]) #Too heavy

## 5) Analysis S3 ####

## Bounding box 
#lapply(G1D1@polygons, bbox) Find out more
