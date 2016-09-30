# Various examples for downloading the data "bridges.data.version1"

# I recommend to first download a copy of the data file to your computer
# download.file()

# using read.table()
dat <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1',
                  sep = ",",
                  stringsAsFactors = FALSE,
                  col.names = c('id', 'river', 'location', 'erected', 'purpose',
                                'length', 'lanes', 'clear', 'tord', 'material',
                                'span', 'rel', 'type'),
                  na.strings = '?')


# using read.csv()
dat2 <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1',
                  stringsAsFactors = FALSE, header = FALSE, row.names = 1,
                  col.names = c('id', 'river', 'location', 'erected', 'purpose',
                                'length', 'lanes', 'clear', 'tord', 'material',
                                'span', 'rel', 'type'),
                  na.strings = '?')


# using scan()
dat <- scan(file = 'http://archive.ics.uci.edu/ml/machine-learning-databases/bridges/bridges.data.version1',
            what = list('character', 'character', 'integer', 'integer',
                        'character', 'numeric', 'numeric', 'character',
                        'character', 'character', 'character', 'character', 'character'),
            sep = "\\ |\\", nlines = 1)


# Challenge: How would you read the "aviation-2015.txt" data
download.file('https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/data/aviation-2015.txt',
  "aviation-2015.txt")

