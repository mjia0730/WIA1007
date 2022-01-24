library(dataMaid)


dataset02 <- read.csv("dataset02.csv")

Malaysian_University_Dataset <- data.frame(dataset02)

attr(Malaysian_University_Dataset$X, "shortDescription") <- "Index"
attr(Malaysian_University_Dataset$course, "shortDescription") <- "University course name"
attr(Malaysian_University_Dataset$Link, "shortDescription") <- "Link to the official website of the course"
attr(Malaysian_University_Dataset$Introduction, "shortDescription") <- "Overview of the course"
attr(Malaysian_University_Dataset$Address, "shortDescription") <- "Address of the university (campus)"
attr(Malaysian_University_Dataset$Contact, "shortDescription") <- "University / faculty / course contact"
attr(Malaysian_University_Dataset$Duration, "shortDescription") <- "Normal course duration (year)"
attr(Malaysian_University_Dataset$Fee, "shortDescription") <- "Complete course tuition fees (RM)"
attr(Malaysian_University_Dataset$uni_name, "shortDescription") <- "The name of the university providing the course"
attr(Malaysian_University_Dataset$Fac_link, "shortDescription") <- "Link to the official faculty website"

makeCodebook(Malaysian_University_Dataset, replace = TRUE)
