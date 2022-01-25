ui <- fluidPage(theme = solar_theme,
                navbarPage("University Course Finder",
                           tabPanel("Finder", #First tab of the shiny app with functions 
                                    #to find course and details of the course
                                    # Set theme
                                              #set the font
                                              tags$style("label{font-family: BentonSans Book}"),
                                              #set gradient background color
                                              #setBackgroundColor(
                                              #  color = c("#F7FBFF", "#2171B5"),
                                              #  gradient = "radial",
                                              #  direction = c("top", "left")
                                              #),
                                              sidebarLayout(
                                                #sidebar with input
                                                sidebarPanel(
                                                  #First input where the user can select 
                                                  #the university
                                                  selectInput(inputId = "uni", label="University", choices = data$uni_name),
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
                                    )),
                           tabPanel("Numbers at A Glance", #Second tab to show plots
                                    #Input where the user can select the university 
                                    #they wanted to search
                                    selectInput(inputId = "uni1", label="University", choices = faculty$uni_name),
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
                                      h4(description_1),
                                      h4(description_2),
                                      h4(description_3)
                                    )),
                           tabPanel("Contributors", #Fourth tab to show the contributors
                                    #of this shiny app
                                    mainPanel(
                                      h1("Main Contributors"),
                                      h4("OOI JIA MING (U2102759)"),
                                      h4("TAN ZI AN (U2102755)"),
                                      h4("TAN XU YANG (U2102862)"),
                                      h4("MUHAMMAD ADAM MALIQUE BIN ZAINAL HABSAHRI (U2102866)")
                                    ))
)











# Define UI for application 
ui <- navbarPage("University Course Finder",
                 tabPanel("Finder", #First tab of the shiny app with functions 
                          #to find course and details of the course
                          # Set theme
                          fluidPage(theme = solar_theme,
                                    #set the font
                                    tags$style("label{font-family: BentonSans Book}"),
                                    #set gradient background color
                                    #setBackgroundColor(
                                    #  color = c("#F7FBFF", "#2171B5"),
                                    #  gradient = "radial",
                                    #  direction = c("top", "left")
                                    #),
                                    sidebarLayout(
                                      #sidebar with input
                                      sidebarPanel(
                                        #First input where the user can select 
                                        #the university
                                        selectInput(inputId = "uni", label="University", choices = data$uni_name),
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
                          )),
                 tabPanel("Numbers at A Glance", #Second tab to show plots
                          #Input where the user can select the university 
                          #they wanted to search
                          selectInput(inputId = "uni1", label="University", choices = faculty$uni_name),
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
                            h4(description_1),
                            h4(description_2),
                            h4(description_3)
                          )),
                 tabPanel("Contributors", #Fourth tab to show the contributors
                          #of this shiny app
                          mainPanel(
                            h1("Main Contributors"),
                            h4("OOI JIA MING (U2102759)"),
                            h4("TAN ZI AN (U2102755)"),
                            h4("TAN XU YANG (U2102862)"),
                            h4("MUHAMMAD ADAM MALIQUE BIN ZAINAL HABSAHRI (U2102866)")
                          ))
                 
)









