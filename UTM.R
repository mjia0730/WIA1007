library(rvest)
library(dplyr)

# Address
jb_campus_address = "UNIVERSITI TEKNOLOGI MALAYSIA, 81310 Skudai, Johor Bahru, Johor, Malaysia."
kl_campus_address = "UNIVERSITI TEKNOLOGI MALAYSIA 81310 Skudai, Johor Bahru, Johor, Malaysia."

# Contacts
jb_campus_contacts = "Phone : +6 07-553 3333\nEmail : corporate@utm.my\nhttps://www.utm.my/"
kl_campus_contacts = "Phone : +6 03-2615 4100\nEmail : corporate.kl@utm.my\nhttps://kl.utm.my/"

# Retrieving Jb campus website
jb_campus_link = "https://admission.utm.my/offered-utmjb-ug-malaysian/"
jb_campus_page = read_html(jb_campus_link)

# Retrieving course names
jb_course_names_0 = jb_campus_page %>% html_nodes(".et_pb_module_header a") %>% html_text()
jb_course_names_1 = jb_campus_page %>% 
                      html_nodes(".et_pb_module_header span") %>% html_text()
jb_course_names = c(jb_course_names_0, jb_course_names_1)

# Retrieving course website links
jb_course_links_0 = jb_campus_page %>% html_nodes(".et_pb_module_header a") %>%
                        html_attr("href")
jb_course_links_1 = jb_campus_page %>% html_nodes(".et_pb_module_header span") %>%
                        html_attr("href")
jb_course_links = c(jb_course_links_0, jb_course_links_1)



# fac of engineering
get_overview_1 = function(alink){
  download.file(alink, destfile = "C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  temp_page = read_html("C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  test_overview = temp_page %>% html_nodes("p, .et_pb_text_inner, p:nth-child(3) , table+ p:nth-child(2)") %>% html_text()
  test_out = ""
  for(ovv in test_overview){
    if(grepl("Bachelor", ovv) & nchar(ovv) > 150){
      test_out = test_out %>% paste(., ovv, sep = " ")
    }
  }
  return(test_out)
}

overviews_1 = sapply(jb_course_links[1:22], get_overview_1)
length(overviews_1)
overviews_1


# fac of build env
get_overview_2 = function(alink){
  download.file(alink, destfile = "C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  temp_page = read_html("C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  test_overview = temp_page %>% html_nodes(".et_pb_accordion_item_0 p") %>% html_text()
  test_out = ""
  for(ovv in test_overview){
    if(nchar(ovv) > 150){
      test_out = test_out %>% paste(., ovv, sep = "\n")
    }
  }
  return(test_out)
}

# Getting course overviews
overviews_2 = sapply(c(jb_course_links[30:34], jb_course_links[36:38]), get_overview_2)
overviews_2 = append(overviews_2, NA, after = 5)


# fac of science
overviews_3 = c(rep(NA, 8))
length(overviews_3)



# business school
get_overview_4 = function(alink){
  download.file(alink, destfile = "C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  temp_page = read_html("C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  test_overview = temp_page %>% html_nodes(".et_pb_text_inner li") %>% html_text()
  test_out = ""
  for(ovv in test_overview){
    test_out = test_out %>% paste(., ovv, sep = "\n")
  }
  return(test_out)
}

overviews_4 = sapply(jb_course_links[47:49], get_overview_4)
overviews_4



# courses with no links
overviews_5 = c(rep(NA, 4))


# courses with no overviews (fac of social science)
overviews_6 = c(rep(NA, 7))

# Adding all the overviews
all_jb_overviews = c(overviews_1, overviews_6, overviews_2, overviews_3, overviews_4, overviews_5)
length(all_jb_overviews)




# Funtion to retrieve course fees
set_fees = function(name){
  fee = 0
  if(grepl("Engineering", name) & !grepl("Computer", name)){
    fee = 12400
  }else if(grepl("Computer", name)){
    fee = 11960
  }else if(grepl("Education", name)){
    fee = 11960
  }else if(grepl("Bachelor of Education", name)){
    fee = 11960
  }else if(grepl("Quantity Surveying", name)){
    fee = 12280
  }else if(grepl("Bachelor of Science in Architecture", name)){
    fee = 9510
  }else if(grepl("Bachelor of Science", name)){
    fee = 11960
  }else{
    fee = 11080
  }
  return(fee)
}

# fees
jb_course_fees = sapply(jb_course_names, set_fees)
length(jb_course_fees)




###### kl campus
kl_campus_link = "https://admission.utm.my/offered-utmkl-ug-malaysian/"
kl_campus_page = read_html(kl_campus_link)
kl_course_names = kl_campus_page %>% html_nodes(".et_pb_module_header a") %>% html_text()
kl_course_links = kl_campus_page %>% html_nodes(".et_pb_module_header a") %>% html_attr("href")


kl_course_fees = c(9510, rep(40000, 4))


kl_overview_1 = "Industrial design is the integration of aesthetics, technology, ergonomic and marketing - know- how into new products, in which needs of people are transformed into potentially commercially products.
\nThe Industrial Design programme curriculum at Razak Faculty of Technology and Informatics as a whole can be regarded as one big creative-course, directed towards new products as commercially and business-oppournities. Through a series of design-projects, which is the core of the curriculum, the students get to go through a creative problem-solving design attitude. This inculcates the attitude that evokes interest in applicable know-how and skills. The curriculum is balanced in such away, that knowledge and skills learned through courses can almost immediately be applied in the product design project.
\nThe industrial design is a four-year programme. There is a internship in the third year with company locally and planning abroad. The education model we apply at Industrial Design Razak Faculty of Technology and Informatics is unique in Malaysia. Students will work in a simulated professional design environment, where they work in groups and individuals. The students are also given related individual assignments. About 80% of the student's time is spent on the core courses, about 20% on university compulsory courses in which students acquire basic knowledge and skills. The programme course consists of basic training and subsequently specialisation in different areas.
\nStudents are thoroughly introduced to the design profession in the first year, so they will find out quite quickly whether they are suited to it or not. All students develop a digital portfolio detailing their experience during the course of their studies showing what they have created or learnt. Students are graded on their portfolios and formal examinations. Successful completion of the programme in Industrial Design results in Bachelor of Science with Honours Industrial Design."


get_kl_overview_2 = function(alink){
  download.file(alink, destfile = "C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  temp_page = read_html("C:/Users/ziant/Desktop/Y1S1/WIA1007 Data Science/Group Project/Scraping/UTM/Scrap.html")
  test_overview = temp_page %>% html_nodes("p") %>% html_text()
  for(ovv in test_overview){
    if(grepl("Engineering ", ovv) && nchar(ovv) > 80){
      return(ovv)
    }
  }
  return(NA)
}

kl_overview_2 = sapply(kl_course_links[-c(1, 5)], get_kl_overview_2)
kl_overview_2

kl_overview_3 = "Software Engineering uses an engineering approach in the development, operation and maintenance of large scale software. A software engineer needs to be able to employ systematic technical and management methods in the creation of high quality software. The Bachelor of Computer Science specializing in Software Engineering is designed to support the nation's need for professional and capable software engineers to undertake the task of increasing the effectiveness and performance of both the public and private sectors. To further support this goal, the course is closely associated with the Malaysian Software Testing Board (MSTB) certifications and Hewlett-Packard (HP) Software Testing Program."
all_kl_overviews = c(kl_overview_1, kl_overview_2, kl_overview_3)


## durations
get_duration = function(name){
  if(grepl("Bachelor of Science in Architecture", name)){
    dur = 3
  }else{
    dur = 4
  }
  return(dur)
}


###### Combine
all_course_names = c(jb_course_names, kl_course_names)
uni_names = c(rep("Universiti Teknologi Malaysia Johor Bahru Campus", 53), 
              c(rep("Universiti Teknologi Malaysia Kuala Lumpur Campus", 5)))
all_course_fees = c(jb_course_fees, kl_course_fees)
all_course_overviews = c(all_jb_overviews, all_kl_overviews)
all_course_duration = sapply(all_course_names, get_duration)
all_course_links = c(jb_course_links, kl_course_links)
adresses = c(rep(jb_campus_address, 53), rep(kl_campus_address, 5))
contacts = c(rep(jb_campus_contacts, 53), rep(kl_campus_contacts, 5))

# Data frame
final_df = data.frame(all_course_names, uni_names, all_course_fees, 
                      all_course_overviews, all_course_duration, all_course_links,
                      adresses, contacts)


View(final_df)
# df = data.frame(jb_course_names, jb_course_fees, all_jb_overviews, jb_course_links, stringsAsFactors = FALSE)
View(df)
#write.csv(final_df, "UTM.csv")




