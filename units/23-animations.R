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
dat <- read.csv("clean-storms-2005.csv")

saveHTML({
  ani.options(interval = 0.1, nmax = 50)  # create 50 image frames
  for (i in 1:200) {
    map("world", col = "gray50", bg = "gray95")
    points(dat$longitude[1:i], dat$latitude[1:i], pch = ".", 
           cex = 2, col = "blue")
  }}, 
  img.name = "storms_plot", ani.height = 500, ani.width = 550,
  title = "Storm Tracks"
)


