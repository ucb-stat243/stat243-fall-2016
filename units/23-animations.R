# Animation paper
# https://www.jstatsoft.org/article/view/v053i01/v53i01.pdf
#
# website gallery
# http://vis.supstat.com/categories.html#animation-ref


# install package "animation"
library(devtools)
install_github('yihui/animation')

library(animation)

# Example 1: Browninan motion
brownian.motion()

# try it again but this time using R from the command line
brownian.motion()

# save animation as html
saveHTML({
  ani.options(interval = 0.05, nmax = 50)  # create 50 image frames
  set.seed(20121106)
  brownian.motion(n = 20, pch = 21, cex = 4, col = "red", bg = "yellow", 
                  xlim = c(-10, 10), ylim = c(-15, 15))
  }, 
  img.name = "brownian-motion", ani.height = 300, ani.width = 550,
  title = "Demonstration of Brownian Motion"
)


# Example 2: Trajectory of Storms
# data in github repo, see "data/" folder
library(animation)
library(rworldmap)

dat <- read.csv("clean-storms-2005.csv")

newmap <- getMap(resolution = "low")

na_lon <- which(dat$longitude <= -20 & dat$longitude >= -140)
na_lat <- which(dat$latitude >= 0 & dat$latitude <= 80)
north <- intersect(na_lon, na_lat)

plot(newmap, xlim = c(-140, -20), ylim = c(0, 80), asp = 1,
     col = "gray95", bg = "#dde9f2", border = "#b5c5d0")
points(dat$longitude[north], dat$latitude[north], col = "#6591ce55", 
       pch = 20, cex = dat$wind_speed[north]/40)

saveHTML({
  ani.options(interval = 0.1, nmax = 50)  # create 50 image frames
  for (i in 1:1000) {
    plot(newmap, xlim = c(-140, -20), ylim = c(0, 80), asp = 1,
         col = "gray95", bg = "#dde9f2", border = "#b5c5d0")
    points(dat$longitude[1:north[i]], dat$latitude[1:north[i]], 
           col = "#6591ce90", pch = 20, 
           cex = dat$wind_speed[1:north[i]]/40)
  }}, 
  img.name = "storms_plot", ani.height = 500, ani.width = 600,
  title = "Storm Tracks"
)

