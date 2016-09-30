# Sample code

# Packages
library(babynames)
library(ggplot2)
library(stringr)

# name to explore (change this value)
myname <- "Gaston"

# we could use str_to_title()
#myname <- str_to_title("gaston")

# subset babynames
names <- subset(babynames, name == myname)

# quick inspection
head(names)
summary(names)

# graph of the number of people named 'myname'
ggplot(data = names, aes(x = year, y = n)) +
  geom_line(aes(color = sex), size = 1) + 
  theme_bw()


# most popular year of 'myname'
names$year[which.max(names$n)]

# number of babies in most popular year
names$n[which.max(names$n)] 

# number of people named so far with 'myname'
sum(names$n) 
