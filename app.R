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
library(shinythemes)

description_1 <- "University Course Finder App is developed to help all the 
                  Malaysian high school graduates to access the information of the courses 
                  provided by Malaysian public universities."


description_2 <- "Information including course overviews, fees, duration and university contacts
                  are available and free to access through the application." 

# Define UI for application 
ui <- navbarPage("University Course Finder",
                 tabPanel("Finder",
                          fluidPage(theme = shinytheme("readable"),
                            tags$style("label{font-family: BentonSans Book}"),
                            #set gradient background color
                            setBackgroundColor(
                              color = c("#F7FBFF", "#2171B5"),
                              gradient = "radial",
                              direction = c("top", "left")
                            ),
                            sidebarLayout(
                              #sidebar with input
                              sidebarPanel(
                                selectInput(inputId = "uni", label="University", choices = data$uni_name),
                                actionButton("check", "Search"),
                                uiOutput("secondSelection"),
                                imageOutput("logo")
                              ),
                              mainPanel(
                                  h3("Introduction"),
                                  textOutput("Introduction"),
                                  h3("Address"),
                                  textOutput("Address"),
                                  h3("Contact"),
                                  textOutput("Contact"),
                                  h3("Duration"),
                                  textOutput("Duration"),
                                  h3("Fee"),
                                  textOutput("Fee"),
                                  h3("Link"),
                                  uiOutput("Link")
                              )
                            )
                          )),
                 tabPanel("Number of courses in each faculty of university",
                          selectInput(inputId = "uni1", label="University", choices = faculty$uni_name),
                          plotOutput("plot1")),
                 tabPanel("About",
                          mainPanel(
                            h1("Welcome to University Course Finder App!"),
                            h4(description_1),
                            h4(description_2)
                          )),
                 tabPanel("Contributors",
                          mainPanel(
                            h1("Main Contributors"),
                            h4("OOI JIA MING"),
                            h4("TAN ZI AN"),
                            h4("XUYANG"),
                            h4("MALIQUE"),
                            h4("INSTRUCTOR:  DR.SALIMAH")
                          ))
  
)

# Define server logic required 
server <- function(input, output, session) {
  observeEvent(input$check , {
    output$secondSelection <- renderUI({
      selectInput("course", "Course:", choices = as.character(data[data$uni_name==input$uni,"course"]))
    })
  })
  observeEvent(input$check, {
    output$Introduction <- renderText({
      data[data$course==input$course, "Introduction"]
    })
    
    output$Address <- renderText({
      data[data$course==input$course, "Address"]
    })
    
    output$Contact <- renderText({
      data[data$course==input$course, "Contact"]
    })
    
    output$Duration <- renderText({
      data[data$course==input$course, "Duration"]
    })
    
    output$Fee <- renderText({
      data[data$course==input$course, "Fee"]
    })
    
    output$Link <- renderUI({
      link <- data[data$course==input$course, "Link"]
      urls <- a("Course Website", href=link)
      tagList("", urls)
    })

  })

  library(ggplot2)
  output$plot1 <- renderPlot({
    if(input$uni1 == "University of Malaya (UM)"){
      ggplot(FacUM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    } 
    else if(input$uni1 == "Universiti Sains Malaysia (USM)"){
      ggplot(FacUSM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Kebangsaan Malaysia (UKM)"){
      ggplot(FacUKM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Teknologi Malaysia (UTM)"){
      ggplot(FacUTM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Utara Malaysia (UUM)"){
      ggplot(FacUUM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Sabah (UMS)"){
      ggplot(FacUMS, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Terengganu (UMT)"){
      ggplot(FacUMT, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Sarawak (UNIMAS)"){
      ggplot(FacUNIMAS, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Pahang (UMP)"){
      ggplot(FacUMP, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Perlis (UniMAP)"){
      ggplot(FacUNIMAP, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Malaysia Kelantan (UMK)"){
      ggplot(FacUMK, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Tun Hussein Onn Malaysia (UTHM)"){
      ggplot(FacUTHM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "UNIVERSITI TEKNIKAL MALAYSIA MELAKA (UTeM)"){
      ggplot(FacUTEM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Pendidikan Sultan Idris (UPSI)"){
      ggplot(FacUPSI, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Sultan Zainal Abidin"){
      ggplot(FacUnisza, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
    else if(input$uni1 == "Universiti Pertahanan Nasional Malaysia (UPNM)"){
      ggplot(FacUPNM, aes(x=Faculty,y=Number.of.courses)) +
        geom_point(size = 3, colour = "black") + 
        geom_segment( aes(x=Faculty, xend=Faculty, y=0, yend=Number.of.courses))+
        labs(y= "Number of courses", x="Faculty")+ coord_flip()
    }
  })
  
  observeEvent(input$check,{
    output$logo <- renderImage({
      if (is.null(input$uni))
        return(NULL)
      if (input$uni == "Universiti Malaya") {
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
      } else if (input$uni == "Universiti Teknologi Malaysia") {
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
      }else if (input$uni == "Universiti Pendidikan Sultan Idris") {
        return(list(
          src = "UPSIlogo.png",
          contentType = "image/png",
          width = 270,
          height = 145,
          alt = "Universiti Pendidikan Sultan Idris"
        ))
      } else if (input$uni == "Universiti Sultan Zainal Abidin") {
        return(list(
          src = "UNISZAlogo.png",
          contentType = "image/png",
          width = 270,
          height = 145,
          alt = "Universiti Sultan Zainal Abidin"
        ))
      } else if (input$uni == "Universiti Pertahanan Nasional Malaysia") {
        return(list(
          src = "UPNMlogo.png",
          contentType = "image/png",
          width = 270,
          height = 145,
          alt = "Universiti Pertahanan Nasional Malaysia"
        ))
      } else if (input$uni == "Universiti Teknikal Malaysia Melaka") {
        return(list(
          src = "UTeMlogo.png",
          contentType = "image/png",
          width = 145,
          height = 145,
          alt = "Universiti Teknikal Malaysia Melaka"
        ))
      }
      
    }, deleteFile = FALSE)
  })

  
}
# Run the application 
shinyApp(ui = ui, server = server)
