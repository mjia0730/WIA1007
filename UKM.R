library(rvest)
library(dplyr)
library(readr)
library(DT)

# UKM website link and website
link = "https://www.ukm.my/studyukm/find-your-programme/"
page = read_html(link)

# From previous scraping 
data = read.csv("um.csv")
View(data)

# GEtting main address
address = page %>% html_nodes(".elementor-element-10096026 p") %>% html_text()

# Getting contacts
contacts = page %>% html_nodes(".elementor-widget-container a") %>% html_attr("href") %>% .[235:238] 
contacts_cut = paste(contacts, collapse = '\n') 

# Getting course names and links
course_names = page %>% html_nodes("td a") %>% html_text()
course_links = page %>% html_nodes("td a") %>% html_attr("href")

df = data.frame(course_names, course_links)

#Subsetting df by faculty
faculties = rename(subset(df, grepl("faculty", course_links)), fac_names=course_names, fac_links=course_links)
courses = subset(df, !grepl("faculty", course_links))
# undergrad = courses[c(53:112),]
undergrad = data
links = undergrad$course_links

# Grab overviews
overviews = c()
for(i in 1:60){
  temp_page = read_html(links[i])
  over = temp_page %>% html_nodes(".elementor-text-editor p") %>% html_text()
  overviews = append(overviews, over[1])
}
View(overviews)
undergrad$overviews = overviews
View(undergrad)


get_info <- function(link){
  download.file(link, destfile = "scapePage.html")
  pg = read_html("scapePage.html")
  texts = pg %>% html_nodes(".elementor-widget-container p") %>% html_text()
  fee = parse_number(texts[grepl("Fees", texts)][1])
  dur = parse_number(texts[grepl("Duration", texts)])
  return(c(fee, dur))
}

info = sapply(links[1:60], get_info)
info_df = data.frame(info)
transpose_df = as.data.frame(t(info_df))
local_fees = transpose_df$V1
durations = transpose_df$V2


undergrad$Local_fees = local_fees
undergrad$Duration = durations
undergrad$Address = c(rep(address, 60))
undergrad$Contacts = c(rep(contacts_cut, 60))
View(undergrad)

datatable(undergrad)





## Sort
undergrad$course_names = trimws(undergrad$course_names, which = c("left"))
sorted_undergrad = arrange(undergrad, course_names)
View(sorted_undergrad)

fin_df = rename(sorted_undergrad, Index=X)
fin_df$Index = c(1:60)
fin_df$University_Name = c(rep("Universiti Kebangsaan Malaysia (UKM)", 60))
View(fin_df)


# write.csv(fin_df, "ukm.csv", sep = "\t")