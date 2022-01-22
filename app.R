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

# Define UI for application 
ui <- navbarPage("University Course Finder",
                 tabPanel("Finder",
                          fluidPage(
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
                                uiOutput("secondSelection"),
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
                                  h3
                                  ("Link"),
                                  textOutput("Link")
                              )
                            )
                          )),
                 tabPanel("Number of courses in each faculty of university",
                          selectInput(inputId = "uni1", label="University", choices = faculty$uni_name),
                          plotOutput("plot1"))
  
)

# Define server logic required 
server <- function(input, output, session) {
  output$secondSelection <- renderUI({
    selectInput("course", "Course:", choices = as.character(data[data$uni_name==input$uni,"course"]))
  })

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
  
  output$Link <- renderText({
    data[data$course==input$course, "Link"]
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
}
# Run the application 
shinyApp(ui = ui, server = server)
