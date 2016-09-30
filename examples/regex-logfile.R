# Working with regular expressions on the log file "may-logs.txt"


# change this directory!!!
setwd('~/Dropbox/course_stat243/labs/logfiles')

library(stringr)

# how to read the data?
# one option is to read in the content with 'readLines()'
logs <- readLines('may-logs.txt')

# take a peek to the contents in logs
head(logs)

# subset a sample of lines
set.seed(98765)
s <- sample(1:length(logs), size = 50)
sublogs <- logs[s]

# matching "jpg"
grep("jpg", sublogs)
grep("jpg", sublogs, value = TRUE)

# we could try to be more precise and match ".jpg"
grep("\\.jpg ", sublogs, value = TRUE)

# let's apply the search on the entire file
jpgs <- grep("\\.jpg ", logs)
length(jpgs)

pngs <- grep("\\.png ", logs)
length(pngs)
gifs <- grep("\\.gif ", logs)
length(gifs)


# matching "jpg" or "png" or "gif"
grep("[jpg][pni][ggf]", sublogs, value = TRUE)

images <- grep("\\.[jpg][pni][ggf]", logs)
length(images)


# Success status code
grep(" 200 ", sublogs, value = TRUE)
grep(" 20[0-9] ", sublogs, value = TRUE)
success <- grep(" 20[0-9] ", logs, value = TRUE)
length(success)

# Redirection status code
grep(" 300 ", sublogs, value = TRUE)
grep(" 3[0-9][0-9] ", sublogs, value = TRUE)
redirections <- grep(" 3[0-9][0-9] ", logs, value = TRUE)
length(redirections)

# client error status code
grep(" 400 ", sublogs, value = TRUE)
grep(" 4[0-9][0-9] ", sublogs, value = TRUE)
errors1 <- grep(" 4[0-9][0-9] ", logs, value = TRUE)
length(errors1)

# server error status code
grep(" 500 ", sublogs, value = TRUE)
grep(" 5[0-9][0-9] ", sublogs, value = TRUE)
errors2 <- grep(" 5[0-9][0-9] ", logs, value = TRUE)
length(errors2)

# most common type of client error status code
error_codes <- str_extract(errors1, " 4[0-9][0-9] ")
table(error_codes)


# Day of the month with more visits

# extracting first word
str_extract(sublogs[1:4], "\\w+")
# two first words
str_extract(sublogs[1:4], "\\w+\\.\\w+")
# one possibility
str_extract(sublogs[1:4], "([-[:alnum:]]+\\.)+[-[:alnum:]]+")
address <- str_extract(logs, "([-[:alnum:]]+\\.)+[-[:alnum:]]+")

# matching dates
str_extract(sublogs[1:4], "\\[[0-9][0-9]/[A-Z][a-z][a-z]/2001")
str_extract(sublogs[1:4], "[0-9][0-9]/[A-Z][a-z][a-z]/2001")
dates <- str_extract(logs, "[0-9][0-9]/[A-Z][a-z][a-z]/2001")

# construct "visits"
visits <- unique(paste(address, dates))
# now let's get the days
days <- str_extract(visits, " [0-9][0-9]")
head(days)
tail(days)

sort(table(days), decreasing = TRUE)
