#Spatial autocorrelation, calculation of Moran's I
########################################################
########################################################

#Source:
#https://rspatial.org/raster/analysis/3-spauto.html
########################################################

#Package installation and data upload
install.packages("raster")
install.packages("rgdal")
install.packages("spdep")
install.packages("FRK")
library(raster)
library(rgdal)
library(spdep)
library(FRK)

#This script can analyze at least six LiDAR images at once!
df1<-read.csv("df1.csv")
df2<-read.csv("df2.csv")
df3<-read.csv("df3.csv")
df4<-read.csv("df4.csv")
df5<-read.csv("df5.csv")
df6<-read.csv("df6.csv")
#Do not remove the first column.  These are keys.

#Visualize the shape of the LiDAR image
plot(df1, col=3:4)

########################################################

#Compute average of the Z-scores and the standard deviation
#This data will help you determine whether there is significant variation in the smoothness of the soil
mean(df1$zcor1)
mean(df2$zcor2)
mean(df3$zcor3)
mean(df4$zcor4)
mean(df5$zcor5)
mean(df6$zcor6)

sd(df1$zcor1)
sd(df2$zcor2)
sd(df3$zcor3)
sd(df4$zcor4)
sd(df5$zcor5)
sd(df6$zcor6)

########################################################

#Compute Moran's I
#This code will let you look at the soil from the side (as if you were putting your face to the ground
#Moran's I is a statistic that will give you an estimate of the soil roughness

#Calculate how close points are to each other;
  #First, convert the data frame to a SpatialPolygons object
    #https://rdrr.io/cran/FRK/man/df_to_SpatialPolygons.html
  #Warnings will appear due to missing data points, but
    #holes in 3D images are unavoidable due to the nature of LiDAR data
    #Despite the warning, chunkier soils consistently had lower Moran's I values!
df1poly<-df_to_SpatialPolygons(df1, "X", c("ycor1", "zcor1"), CRS())
df2poly<-df_to_SpatialPolygons(df2, "X", c("ycor2", "zcor2"), CRS())
df3poly<-df_to_SpatialPolygons(df3, "X", c("ycor3", "zcor3"), CRS())
df4poly<-df_to_SpatialPolygons(df4, "X", c("ycor4", "zcor4"), CRS())
df5poly<-df_to_SpatialPolygons(df5, "X", c("ycor5", "zcor5"), CRS())
df6poly<-df_to_SpatialPolygons(df6, "X", c("ycor6", "zcor6"), CRS())

#View the data in spatial polygon format
plot(df1poly)

#Next, calculate polygon adjacency
w1 <- poly2nb(df1poly)
w2 <- poly2nb(df2poly)
w3 <- poly2nb(df3poly)
w4 <- poly2nb(df4poly)
w5 <- poly2nb(df5poly)
w6 <- poly2nb(df6poly)
class(w1) #Should say "nb"
summary(w2)
#Summary output should describe the average number of neighboring polygons each polygon has
str(w1) #For more information, check the structure

#Calculate the spatial weights matrix
ww1<-nb2listw(w1, style='B', zero.policy = TRUE)
ww2<-nb2listw(w2, style='B', zero.policy = TRUE)
ww3<-nb2listw(w3, style='B', zero.policy = TRUE)
ww4<-nb2listw(w4, style='B', zero.policy = TRUE)
ww5<-nb2listw(w5, style='B', zero.policy = TRUE)
ww6<-nb2listw(w6, style='B', zero.policy = TRUE)

#Get Moran's I values by running Monte Carlo iterations
    #High values of Moran's I mean the soil is more homogeneous
    #Significance statistics are also reported
moran.mc(df1$zcor1, ww1, nsim=99, zero.policy = TRUE)
moran.mc(df2$zcor2, ww2, nsim=99, zero.policy = TRUE)
moran.mc(df3$zcor3, ww3, nsim=99, zero.policy = TRUE)
moran.mc(df4$zcor4, ww4, nsim=99, zero.policy = TRUE)
moran.mc(df5$zcor5, ww5, nsim=99, zero.policy = TRUE)
moran.mc(df6$zcor6, ww6, nsim=99, zero.policy = TRUE)

#End####################################################
