#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library(shinyWidgets)
library(shinythemes) # styling
library(thematic)
library(bslib)
library(dplyr) # for data handling

custom_theme <- bs_theme(
  # set colors
  bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198", secondary = "#48DAC6",
  # bslib also makes it easy to import CSS fonts
  base_font = font_google("Fira Sans"),
  heading_font = font_google("Fredoka One")
)

#Description to the shiny app is assign the variables "description_1" and "description_2"
description_1 <- "University Course Finder App is developed to help all the 
                  Malaysian university applicants to access the official information of the courses 
                  provided by Malaysian public universities easily."

description_2 <- "Information including course overviews, fees, duration and university contacts
                  are available and free to access through the application." 

description_3 <- "Find detailed course and university information in the 'University Course Finder'tab
                & get the visualisations of the information in the 'Numbers at A Glance tab'"


ui <- fluidPage(theme = custom_theme, # Set theme
                tags$style("
                  p {
                    text-align: justify;
                    text-justify: inter-word;
                  }
                           "),
                navbarPage("University Course Finder",
                           tabPanel("Finder", #First tab of the shiny app with functions 
                                    #to find course and details of the course
                                    sidebarLayout(
                                      #sidebar with input
                                      sidebarPanel(
                                        #First input where the user can select 
                                        #the university
                                        selectInput(inputId = "uni", label="University", 
                                                    choices = c("-Select University-" = "", data$uni_name),
                                                    selectize = TRUE),
                                        #Second input where the user can select course 
                                        #based on the university of first input
                                        uiOutput("secondSelection"),
                                        #An action button for the user to press
                                        actionButton("check", "Search"),
                                        #Output of the logo of university selected 
                                        #by user at the first input
                                        imageOutput("logo")
                                      ),
                                      mainPanel( #At the main panel, it will shows 
                                        #output of the details based on 
                                        #the course and university selected by user
                                        h3("Introduction"),
                                        htmlOutput("Introduction"),
                                        h3("Duration"),
                                        htmlOutput("Duration"),
                                        h3("Fee (Approximate)"),
                                        textOutput("Fee"),
                                        h3("Address"),
                                        textOutput("Address"),
                                        h3("Contact"),
                                        htmlOutput("Contact"),
                                        h3("Official Website"),
                                        uiOutput("Link")
                                      )
                                    )
                           ),
                tabPanel("Numbers at A Glance", #Second tab to show plots
                         #Input where the user can select the university 
                         #they wanted to search
                         selectInput(inputId = "uni1", label="University", 
                                     c("-Select University-" = "", faculty$uni_name)),
                         #Plot button
                         actionButton("checkplot", "Plot!"),
                         h3("Number of courses in each faculty of university"),
                         #Plot showing number of courses in each faculty 
                         #of the university selected by user
                         plotOutput("plot1"),
                         h3("Fees"),
                         #Plot showing the fees of the courses of the 
                         #university selected by user
                         plotOutput("plot2")),
                
                tabPanel("About", #Third tab to show description of the shiny app
                         mainPanel(
                           h1("Welcome to University Course Finder App!"),
                           p(description_1),
                           p(description_2),
                           p(description_3)
                         )),
                tabPanel("Contributors", #Fourth tab to show the contributors
                         #of this shiny app
                         mainPanel(
                           h1("Main Contributors"),
                           p("OOI JIA MING (U2102759)"),
                           p("TAN ZI AN (U2102755)"),
                           p("TAN XU YANG (U2102862)"),
                           p("MUHAMMAD ADAM MALIQUE BIN ZAINAL HABSAHRI (U2102866)")
                         ))
))





# Define server logic required 
server <- function(input, output, session) {
  
    #Filter the selection for second input based on the first input
    output$secondSelection <- renderUI({
      selectInput("course", "Course:",  
                  choices = c("-Select Course-" = "", as.character(data[data$uni_name==input$uni,"course"])),
                  selectize = TRUE)
    })
    
    # check if the actionButton is checked
    observeEvent(input$check, {
      #Show the introduction of the course selected by user
      output$Introduction <- renderText({
        # check if the actionButton is checked (to prevent button from functioning once only)
        input$check 
        # isolate long logics from actionButton check
        isolate({
          HTML(data[data$course==input$course, "Introduction"])
        })
      })
      #Show the faculty's address of the course selected by user
      output$Address <- renderText({
        input$check
        isolate({
          data[data$course==input$course, "Address"]
        })
      })
      #Show the faculty's contact of the course selected by user
      output$Contact <- renderText({
        input$check
        isolate({
          HTML(data[data$course==input$course, "Contact"])
        })
      })
      #Show the duration of the course selected by user
      output$Duration <- renderText({
        input$check
        isolate({
          if(input$course != ""){
            temp_dur <- HTML(data[data$course==input$course, "Duration"])
            if(temp_dur == "NA"){
              HTML("The university has not updated this information during the data collection process.
              Please refer to the latest updates on the university's website.")
            }else{
              HTML(paste(temp_dur, "Years (subjected to changes)"))
            }
          }
        })
      })
      #Show the fee of the course selected by user
      output$Fee <- renderText({
        input$check
        isolate({
          if(input$course != ""){
            temp <- data[data$course==input$course, "Fee"]
            if(is.na(temp)){
              "The university has not updated this information during the data collection process.
        Please refer to the latest updates on the university's website."
            }
            else{
              paste("RM ", temp)
            }
          }
        })
      })
      #Show the website link of the course selected by user
      output$Link <- renderUI({
        input$check
        isolate({
          if(input$course != ""){
            link <- data[data$course==input$course, "Link"]
            urls <- a("Course Website", href=link)
            tagList("", urls)
          }
        })
      })
      #Show the logo of the university selected by user
      output$logo <- renderImage({
        input$check
        isolate({
          
          outfile <- tempfile(fileext = '.png')
          
          # Return null if no university is selected
          if (input$uni == "Universiti Malaya (UM)") {
            # Load and return the university's logo image
            return(list(
              src = "University Malaya.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "University Malaya"
            ))
          } else if (input$uni == "Universiti Sains Malaysia(USM)") {
            return(list(
              src = "USMlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Sains Malaysia"
            ))
          } else if (input$uni == "Universiti Kebangsaan Malaysia (UKM)") {
            return(list(
              src = "UKMlogo.jpg",
              fileType = "image/jpg",
              width = 145,
              height = 145,
              alt = "Universiti Kebangsaan Malaysia"
            ))
          } else if (input$uni == "Universiti Teknologi Malaysia (UTM)") {
            return(list(
              src = "UTMlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Teknologi Malaysia"
            ))
          } else if (input$uni == "Universiti Malaysia Pahang(UMP)") {
            return(list(
              src = "UMPlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Pahang"
            ))
          } else if (input$uni == "Universiti Malaysia Perlis (UniMAP)") {
            return(list(
              src = "UNIMAPlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Perlis"
            ))
          } else if (input$uni == "Universiti Malaysia Kelantan (UMK)") {
            return(list(
              src = "UMKlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Kelantan"
            ))
          } else if (input$uni == "Universiti Tun Hussein Onn Malaysia (UTHM)") {
            return(list(
              src = "UTHMlogo.jpg",
              fileType = "image/jpg",
              width = 145,
              height = 145,
              alt = "Universiti Tun Hussein Onn Malaysia"
            ))
          } else if (input$uni == "Univesity Malaysia Sabah (UMS)") {
            return(list(
              src = "UMSlogo.jpg",
              fileType = "image/jpg",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Sabah"
            ))
          } else if (input$uni == "Univesity Malaysia Terengganu (UMT)") {
            return(list(
              src = "UMTlogo.jpg",
              fileType = "image/jpg",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Terengganu"
            ))
          } else if (input$uni == "Univesity Malaysia Sarawak (UNIMAS)") {
            return(list(
              src = "UNIMASlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Malaysia Sarawak"
            ))
          } else if (input$uni == "Univesity Utara Malaysia (UUM)") {
            return(list(
              src = "UUMlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Utara Malaysia"
            ))
          }else if (input$uni == "Universiti Pendidikan Sultan Idris (UPSI)") {
            return(list(
              src = "UPSIlogo.png",
              contentType = "image/png",
              width = 200,
              height = 145,
              alt = "Universiti Pendidikan Sultan Idris"
            ))
          } else if (input$uni == "Universiti Sultan Zainal Abidin (UNISZA)") {
            return(list(
              src = "UNISZAlogo.png",
              contentType = "image/png",
              width = 200,
              height = 145,
              alt = "Universiti Sultan Zainal Abidin"
            ))
          } else if (input$uni == "Universiti Pertahanan Nasional Malaysia (UPNM)") {
            return(list(
              src = "UPNMlogo.png",
              contentType = "image/png",
              width = 200,
              height = 145,
              alt = "Universiti Pertahanan Nasional Malaysia"
            ))
          } else if (input$uni == "Universiti Teknikal Malaysia Melaka (UTeM)") {
            return(list(
              src = "UTeMlogo.png",
              contentType = "image/png",
              width = 145,
              height = 145,
              alt = "Universiti Teknikal Malaysia Melaka"
            ))
          }
          else{
            return(list(src = outfile,
                  contentType = 'image/png',
                  width = 0,
                  height = 0,
                  alt = ""))
          }
        })
      }, deleteFile = FALSE)
    })
    

  # Import ggplot2 for plotting
  library(ggplot2)
    
    # Check if the actionButton is checked
    observeEvent(input$checkplot,{
      #Plotting a histogram for the fees of the courses of the university selected by user
      output$plot2 <- renderPlot({
        input$checkplot
        
        # Isolate long logics
        isolate({
          if(input$uni1 == "University of Malaya (UM)"){
            sub_data <- data[data$uni_name == "Universiti Malaya (UM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Sains Malaysia (USM)"){
            # Subsetting data for corresponding university for plotting
            sub_data <- data[data$uni_name == "Universiti Sains Malaysia(USM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Kebangsaan Malaysia (UKM)"){
            sub_data <- data[data$uni_name == "Universiti Kebangsaan Malaysia (UKM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Teknologi Malaysia (UTM)"){
            sub_data <- data[data$uni_name == "Universiti Teknologi Malaysia (UTM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Utara Malaysia (UUM)"){
            sub_data <- data[data$uni_name == "Univesity Utara Malaysia (UUM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Sabah (UMS)"){
            sub_data <- data[data$uni_name == "Univesity Malaysia Sabah (UMS)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Terengganu (UMT)"){
            sub_data <- data[data$uni_name == "Univesity Malaysia Terengganu (UMT)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Sarawak (UNIMAS)"){
            sub_data <- data[data$uni_name == "Univesity Malaysia Sarawak (UNIMAS)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Pahang (UMP)"){
            sub_data <- data[data$uni_name == "Universiti Malaysia Pahang(UMP)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Perlis (UniMAP)"){
            sub_data <- data[data$uni_name == "Universiti Malaysia Perlis (UniMAP)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Malaysia Kelantan (UMK)"){
            sub_data <- data[data$uni_name == "Universiti Malaysia Kelantan (UMK)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Tun Hussein Onn Malaysia (UTHM)"){
            sub_data <- data[data$uni_name == "Universiti Tun Hussein Onn Malaysia (UTHM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "UNIVERSITI TEKNIKAL MALAYSIA MELAKA (UTeM)"){
            sub_data <- data[data$uni_name == "Universiti Teknikal Malaysia Melaka (UTeM)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Pendidikan Sultan Idris (UPSI)"){
            sub_data <- data[data$uni_name == "Universiti Pendidikan Sultan Idris (UPSI)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Sultan Zainal Abidin"){
            sub_data <- data[data$uni_name == "Universiti Sultan Zainal Abidin (UNISZA)",]
            plot_data <- sub_data
          }
          else if(input$uni1 == "Universiti Pertahanan Nasional Malaysia (UPNM)"){
            sub_data <- data[data$uni_name == "Universiti Pertahanan Nasional Malaysia (UPNM)",]
            plot_data <- sub_data
          }
          
          if(input$uni1 != ""){
            # Plotting fees using ggplot2 histogram plot
            ggplot(plot_data, aes(x=Fee)) +
              geom_histogram()+
              labs(y= "Number of Courses", x="Fees") +
              # Show meadian fee
              geom_vline(aes(xintercept=median(Fee, na.rm=TRUE)),
                         color="white", linetype="dashed", size=1) +
              geom_text(aes(x=median(Fee, na.rm=TRUE), y= 35, label = "Median Fee"), 
                        colour = "pink", vjust = 1, size = 5)
          }
        })
      })  
      
      #Plotting a lollipop plot for the number of courses of each faculty of the university selected by user
      output$plot1 <- renderPlot({
        
        input$checkplot
        
        # Isolate long logics from actionButton check
        isolate({
          if(input$uni1 == "University of Malaya (UM)"){
            # Lollipop plot
            ggplot(FacUM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          } 
          else if(input$uni1 == "Universiti Sains Malaysia (USM)"){
            ggplot(FacUSM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Kebangsaan Malaysia (UKM)"){
            ggplot(FacUKM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Teknologi Malaysia (UTM)"){
            ggplot(FacUTM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Utara Malaysia (UUM)"){
            ggplot(FacUUM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Sabah (UMS)"){
            ggplot(FacUMS, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Terengganu (UMT)"){
            ggplot(FacUMT, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Sarawak (UNIMAS)"){
            ggplot(FacUNIMAS, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Pahang (UMP)"){
            ggplot(FacUMP, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Perlis (UniMAP)"){
            ggplot(FacUNIMAP, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Malaysia Kelantan (UMK)"){
            ggplot(FacUMK, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Tun Hussein Onn Malaysia (UTHM)"){
            ggplot(FacUTHM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "UNIVERSITI TEKNIKAL MALAYSIA MELAKA (UTeM)"){
            ggplot(FacUTEM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Pendidikan Sultan Idris (UPSI)"){
            ggplot(FacUPSI, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Sultan Zainal Abidin"){
            ggplot(FacUnisza, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
          else if(input$uni1 == "Universiti Pertahanan Nasional Malaysia (UPNM)"){
            ggplot(FacUPNM, aes(x=Faculty,y=Number.of.courses)) +
              geom_point(size = 3, colour = "pink") + 
              geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
              labs(y= "Number of courses", x="Faculty")+ coord_flip()
          }
        })
      })
    })
}

# Enable bslib to render theme 
thematic_shiny()
# Run the application 
shinyApp(ui = ui, server = server)
