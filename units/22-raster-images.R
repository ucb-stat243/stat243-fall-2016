
setwd('~/Desktop/')

library(png)

download.file(
  url = 'https://developer.r-project.org/Logo/Rlogo-5.png',
  destfile = 'rlogo.png'
)

rlogo <- readPNG('rlogo.png')

a <- seq(60, 360, 30)
x <- cos(a/180 * pi) * a/360
y <- sin(a/180 * pi) * a/360

plot(x, y, type = "n")
lines(x, y, lwd = 3, col = '#aaaaaa99')
rasterImage(rlogo, x - 0.07, y - 0.07, x + 0.07, y + 0.07, 
            interpolate = TRUE)


download.file(
  url = 'http://sweetclipart.com/multisite/sweetclipart/files/wine_in_glass.png',
  destfile = 'wine-glass.png'
)

system('convert -resize 20% wine-glass.png glass.png')
glass <- readPNG('glass.png')

plot(x, y, type = "n")
rasterImage(glass, x - 0.07, y - 0.07, x + 0.07, y + 0.07, 
            interpolate = TRUE)

