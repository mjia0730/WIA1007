library(dataMaid)


dataset02 <- read.csv("dataset02.csv")

df <- data.frame(dataset02)

makeCodebook(df)
