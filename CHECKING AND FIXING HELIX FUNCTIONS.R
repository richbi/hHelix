
#----consistent arguments and names
#----correct datetime and time zone specification
#----basic utilities/functionality


rm(list=ls())

library(rgl)
library(R.utils)
library(rgl)
library(colorspace)
library(broman)
library(rgl)
library(suncalc)
library(suntools)


## ------  SOURCE THE REQUIRED FUNCTIONS  ------
ls()
getwd()
sourceDirectory("FUNCTIONS", modifiedOnly=FALSE)
ls()



#############################################
#    UPDATED HELIX CALCULATION FUNCTION
#############################################

hlx<-CalcHelix_NEW(n.loops = 1 ,n.points = 24, radius = 3, plot = TRUE)
rglwidget(width=800,height=400)

hlx<-CalcHelix_NEW(n.loops = c(1,10) ,n.points = 24, radius = c(3,1), plot = TRUE)
rglwidget(width=800,height=400)


hlx<-CalcHelix_NEW(n.loops = c(5,7) ,n.points = 24, radius = c(1,0.5), plot = TRUE)
rglwidget(width=800,height=400)

#############################################
#    UPDATED WRAPPER FUNCTION
#############################################


library(worldmet)
library(suncalc)
library(ggplot2)
library(dplyr)

#-----DISPLAYING CLIMATE DATA

#---DOWNLOAD THE CLIMATE DATA FOR Ås, in Norway
dat <- worldmet::importNOAA(code = "014630-99999", year = 2020)
dat <- worldmet::importNOAA(code = "404370-99999", year = 2020)
dat <- worldmet::importNOAA(code = "53186-999999", year = 2020)# DEATH VALLEY? SOMETHING WRONG WITH THE CODE
dat <- worldmet::importNOAA(code = "606700-99999", year = 2020)

names(dat)
table(is.na(dat$air_temp))

#----SPECIFY CERTAIN HIERARCHICAL HELIX PARAMETERS
lunar.month <- 29.530575#---expressed in days
earth.year <- 365.242374#---expressed in days
earth.day <- 1
earth.week <- 7
periods <- c(earth.year, lunar.month, earth.day)

lonlat <- dat[1, c("longitude", "latitude")] %>% unlist()




airtemp.dat <- dat %>% dplyr::select(date, air_temp) %>% 
  rename(datetime = date, cov = air_temp) %>%
  mutate(month=as.numeric(format(datetime,"%m")))%>%
  filter(month%in%c(3))

start <- min(airtemp.dat$datetime)
end <- max(airtemp.dat$datetime)




temp <- ggplot(airtemp.dat, mapping = aes(x=datetime, y=cov)) +
  geom_point(aes(color = cov), size = 2) +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  )
temp
g <- ggplot_build(temp)
airtemp.dat$col <- g$data[[1]]$colour


hHelix(
  # t = NULL
  # ,
  # t.color = "black"
  # ,
  # t.radius = 0.06#sphere.radius * 1.25
  # ,
  # t.alpha = 1
  # ,
  sphere.radius = 0.06
  ,
  size = as.numeric(difftime(max(airtemp.dat$datetime),min(airtemp.dat$datetime),units="hours"))
  ,
  start = min(airtemp.dat$datetime) #+6*60*60
  ,
  end = max(airtemp.dat$datetime)
  ,
  mark.start = TRUE,
  #-------------------------------------
  radii = c(1, 0.3, 0.15)
  ,
  lonlat = lonlat
  ,
  order = c(1, 2, 3)
  ,
  periods = c(earth.year, lunar.month, earth.day)
  ,
  cov.df = airtemp.dat
  ,
  time.line = c("00:00:00","12:00:00","06:00:00","18:00:00"),
  time.line.color = c("black","white","blue","red"),
  show.lunar = FALSE
  
)

rglwidget(width=800,height=400)



#-----DISPLAYING DAYLIGHT AND MOON LIGHT


dat<-tibble(datetime=seq(as.POSIXct("2020-01-01 00:00:00",tz="Europe/Paris"), as.POSIXct("2020-03-31 00:00:00",tz="Europe/Paris"), by="hour"))

light.dat<-dat%>%mutate(sun.altitude=getSunlightPosition(date = datetime, lat = lonlat[2], lon = lonlat[1],
                                      data = NULL, keep = c("altitude"))$altitude,
                  moon.fraction=getMoonIllumination(date = datetime, keep = c("fraction"))$fraction)



sunglow <- brocolors("crayons")["Sunglow"]
midnightblue <- brocolors("crayons")["Midnight Blue"]

temp <- ggplot(light.dat, mapping = aes(x=datetime, y=sun.altitude)) +
  geom_point(aes(color = sun.altitude), size = 2) +
  scale_color_gradient2(
    low = "red",
    mid = "yellow",
    high = "white",
    midpoint = 0
  )
temp
g <- ggplot_build(temp)
light.dat$col <- g$data[[1]]$colour

temp <- ggplot(light.dat, mapping = aes(x=datetime, y=moon.fraction)) +
  geom_point(aes(color = moon.fraction), size = 2) +
  scale_color_gradient2(
    low = "black",
    mid = "black",
    high = "white",
    midpoint = 0
  )
temp
g <- ggplot_build(temp)

light.dat$col<-ifelse(light.dat$sun.altitude<= -(18 * pi/180), g$data[[1]]$colour,light.dat$col) 

plot(sun.altitude~datetime,data=light.dat,col=col)


hHelix(
  sphere.radius = 0.08
  ,
  size = as.numeric(difftime(max(light.dat$datetime),min(light.dat$datetime),units="hours"))
  ,
  
  #---THESE ARE NOT PASSED
  start = min(light.dat$datetime) +6*60*60
  ,
  end = max(light.dat$datetime)
  ,
  mark.start = TRUE
  ,
  #-------------------------------------
  radii = c(1, 0.3, 0.15)
  ,
  lonlat = lonlat
  ,
  order = c(1, 2, 3)
  ,
  periods = c(earth.year, lunar.month, earth.day)
  ,
   cov.df = light.dat
  ,
  time.line = c("00:00:00","12:00:00","06:00:00","18:00:00"),
  time.line.color = c("black","white","blue","red"),
  show.lunar = FALSE
  
)

rglwidget(width=800,height=400)

#-----INTEGRATED DAYLIGHT/MOONLIGHT
hHelix(
  sphere.radius = 0.08
  ,
  size = as.numeric(difftime(max(airtemp.dat$datetime),min(airtemp.dat$datetime),units="hours"))
  ,
  start = min(airtemp.dat$datetime)
  ,
  end = max(airtemp.dat$datetime)
,
  #-------------------------------------
  radii = c(1, 0.3, 0.15)
  ,
  lonlat = lonlat
  ,
  order = c(1, 2, 3)
  ,
  periods = c(earth.year, lunar.month, earth.day)
  
)

rglwidget(width=800,height=400)






#############################################
#    TRYING WEEKS
#############################################

# 
# #----SPECIFY CERTAIN HIERARCHICAL HELIX PARAMETERS
# lunar.month <- 29.530575#---expressed in days
# earth.year <- 365.242374#---expressed in days
# earth.day <- 1
# earth.week <- 7
# periods <- c(earth.year, earth.week, earth.day)
# 
# lonlat <- dat[1, c("longitude", "latitude")] %>% unlist()
# 
# library(ggplot2)
# #library(tidyr)
# library(dplyr)
# 
# 
# airtemp.dat <- dat %>% dplyr::select(date, air_temp) %>% 
#   rename(datetime = date, cov = air_temp) %>%
#   mutate(month=as.numeric(format(datetime,"%m")))%>%
#   filter(month%in%c(1:6))
# 
# start <- min(airtemp.dat$datetime)
# end <- max(airtemp.dat$datetime)
# 
# 
# 
# 
# temp <- ggplot(airtemp.dat, mapping = aes(1:length(cov), cov)) +
#   geom_point(aes(color = cov), size = 2) +
#   scale_color_gradient2(
#     low = "blue",
#     mid = "white",
#     high = "red",
#     midpoint = 0
#   )
# temp
# g <- ggplot_build(temp)
# airtemp.dat$col <- g$data[[1]]$colour
# 
# 
# 
# WrapHelix(#h=0.1,
#   t = NULL
#   ,
#   t.color = "black"
#   ,
#   t.radius = 0.06#sphere.radius * 1.25
#   ,
#   t.alpha = 1
#   ,
#   sphere.radius = 0.1
#   ,
#   size = as.numeric(difftime(max(airtemp.dat$datetime),min(airtemp.dat$datetime),units="hours"))
#   ,
#   start = min(airtemp.dat$datetime) +6*60*60
#   ,
#   end = max(airtemp.dat$datetime)
#   ,
#   #-------------------------------------
#   radii = c(1, 0.2, 0.1)
#   ,
#   lonlat = lonlat
#   ,
#   order = c(1,2, 3)
#   ,
#   periods = c(earth.year, earth.week, earth.day)
#    ,
#    cov.df = airtemp.dat
#   ,
#   time.line = "00:00:00",
#   time.line.color = "yellow",
#   show.lunar = FALSE
#   
# )
# 
# rglwidget(width=800,height=400)
