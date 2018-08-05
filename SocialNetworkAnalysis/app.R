#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(igraph)
library(shiny)
library(DT)
library(networkD3)
library(visNetwork)
library(plyr)

url_git <- "https://github.com/arpitchaukiyal/Social-Network-Analysis"
ui <- fluidPage(
    tags$h4(align = "right", "ArpitChaukiyal_Social_Network_Analysis"),    
    # This is the code to construct the side bar of the shiny app. This side bar is common
    # all the tabs in this app.
    sidebarLayout(
        sidebarPanel(
            #For asking the user to input the email file.
            fluidRow(
                fileInput(inputId = "email_file", label = "Select Email file",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                )
            ),
            #For asking the user to input the department file.
            fluidRow(
                fileInput(inputId = "dept_file", label = "Select Dept file",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                )
            ),
            #checkbox for weather the file contains header or not.
            fluidRow(checkboxInput(inputId = "header", label = "Header", value = F)),
            
            #for asking user the number of records to show from the input file1.
            fluidRow(numericInput(inputId = "num_connections", 
                                  label = "Number of records to show", min = 1, value = 1000)),
            fluidRow(
                conditionalPanel(
                    condition = "input.tab == 3",
                    radioButtons(inputId = "table_viz", label = "Choose",
                                 choices = list("Table data" = 1,
                                                "2-hop" = 2), selected = 2) #new change and it is working just fine.
                ),
                
                conditionalPanel(
                    condition = "input.tab == 4",
                    radioButtons(inputId = "table_viz1", label = "Choose Table/Graph",
                                 choices = list("Table data" = 1,
                                                "2-hop" = 2))
                ),
                
                conditionalPanel(
                    condition = "input.tab == 5",
                    radioButtons(inputId = "table_viz2", label = "Choose",
                                 choices = list("Table data" = 1,
                                                "2-hop" = 2))
                )
            ),
            width = 3
        ),
        
        mainPanel(
            tabsetPanel(id = "tab",
                        # Frist tab for introduction for this app and links to download the file used as input.
                        tabPanel(title = "Welcome", value =99,
                                 fluidRow(
                                     tags$h3(tags$u("Introduction:")),
                                     tags$div(class = "Intro",
                                              tags$p("This", tags$i("Shiny App"), "to demostrate the social network
                                                     genetared due to email exchange between different departments of 
                                                     a large European research institution. The dataset is a subset 
                                                     of data provided by Stanford Network Analysis Platform", 
                                                     tags$b(tags$a(href="https://snap.stanford.edu/data/email-EuAll.html", target="_blank",
                                                     "(Know more about it!)")), ".")
                                              ),
                                     tags$div(class = "File",
                                              tags$h4(tags$u("DataSet Files:")),
                                              tags$p("To use this app, dataset files need to be downloaded first. Description of the files
                                                     and the links to downdoan them are provided below."),
                                              tags$ul(
                                                  tags$li(tags$b("Email File (email-Eu-core.txt):"),"Contains data generated as the result of email exchanges from a large 
                                                          European research institution. The actual information has been anonymized 
                                                          about all incoming and outgoing email between members of the research institution.",
                                                          tags$a(href="https://drive.google.com/open?id=1_MrrFx7lisc44dPOgqbPB2OJ1Dwk3FZS", target="_blank",
                                                                 "(download here.)")),
                                                  tags$li(tags$b("Dept File (email-Eu-core-department-labels.txt):"), "This file contains the 
                                                          department of the individuals. Each individual belongs to exactly one of 42 departments
                                                          at the research institute. This information is also anonymized.",
                                                          tags$a(href="https://drive.google.com/open?id=1N8o00ROmM6bq0URaXXHJtLeQHVnJYCB9", target="_blank",
                                                                 "(download here.)")
                                                          )
                                                    )
                                                ),
                                     tags$div(class = "File",
                                              tags$h4(tags$u("About Me:"))
                                                  )
                                    )
                                ),
                        
                        # Second tab for displaying the data in tabular from.
                        tabPanel(title = "Data", value = 1,
                                 fluidRow(
                                     #tableOutput(outputId = "nconnectionemail")
                                     column(6, dataTableOutput(outputId = "nconnectionemail")),
                                     column(6, dataTableOutput(outputId = "ndepartment"))
                                 )
                        ),
                        
                        tabPanel(title = "Network Graph", value = 2,
                                 simpleNetworkOutput(outputId = "nconnectionsgraph") 
                        ),
                        
                        tabPanel(title = "Send/Recieve", value = 3,
                                 tabsetPanel(id = "3_innertab",
                                             tabPanel(title = "Sender Information", value = 1,
                                                      conditionalPanel(
                                                          condition = "input.table_viz == 1",
                                                          dataTableOutput(outputId = "highestsender")
                                                      ),
                                                      conditionalPanel(
                                                          condition = "input.table_viz == 2",
                                                          visNetworkOutput(outputId = "hopneighboursender")
                                                      )
                                             ),
                                             
                                             tabPanel(title = "Reciever Information", value = 2,
                                                      conditionalPanel(
                                                          condition = "input.table_viz == 1",
                                                          dataTableOutput(outputId = "highestreciever")
                                                      ),
                                                      conditionalPanel(
                                                          condition = "input.table_viz == 2",
                                                          visNetworkOutput(outputId = "hopneighboursrecv")
                                                      )
                                             )
                                 )
                        ),
                        
                        tabPanel(title = "Centrality", value = 4,
                                 tabsetPanel(id = "4_innertab",
                                             tabPanel(title = "Degree Centrality", value = 1,
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 1",
                                                          dataTableOutput(outputId = "highestdegree")
                                                      ),
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 2",
                                                          visNetworkOutput(outputId = "hopneighbordegree")
                                                      )
                                             ),
                                             
                                             tabPanel(title = "Betweeness Centrality", value = 2,
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 1",
                                                          dataTableOutput(outputId = "highestbetween")
                                                      ),
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 2",
                                                          visNetworkOutput(outputId = "hopneighborbetween")
                                                      )
                                             ),
                                             
                                             tabPanel(title = "In Degree Centrality", value = 3,
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 1",
                                                          dataTableOutput(outputId = "highestindegree")
                                                      ),
                                                      conditionalPanel(
                                                          condition = "input.table_viz1 == 2",
                                                          visNetworkOutput(outputId = "hopneighborindegree")
                                                      )
                                             ),
                                             
                                             tabPanel(title = "Observation", value = 3,
                                                      textOutput(outputId = "observation")
                                             )
                                 )
                        ),
                        
                        tabPanel(title = "Department Level", value = 5,
                                 conditionalPanel(
                                     condition = "input.table_viz2 == 1",
                                     dataTableOutput(outputId = "deptemailtable")
                                 ),
                                 conditionalPanel(
                                     condition = "input.table_viz2 == 2",
                                     visNetworkOutput(outputId = "deptemailgraph")
                                 )
                        )
            )
        )
    )
)


server <- function(input, output){
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #                                   All the Recative blocks of the Shiny App                                        #
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    # Create a reactive block for retrieving the "N" number of connection from the email data
    # file and save it in the email_data variable.
    email_data <- reactive(
        {
            email_file_data <- input$email_file
            if(is.null(email_file_data)){
                #print(file_data)
                return(NULL)
            }
            email_data_set <- read.csv(email_file_data$datapath,
                                       header = input$header, sep = " ")
            colnames(email_data_set) <- c("Sender", "Reciever")
            email_data_set$Sender <- as.character(email_data_set$Sender)
            email_data_set$Reciever <- as.character(email_data_set$Reciever)
            num_of_con <- as.numeric(input$num_connections)
            email_data_sample <- email_data_set[1:num_of_con, ] 
            return(email_data_sample)
        }
    )
    
    # Create a reactive block for retrieving the "N" number of rows from the department data
    # file and save it in the dept_data variable.
    dept_data <- reactive(
        {
            dept_file_data <- input$dept_file
            if(is.null(dept_file_data)){
                return(NULL)
            }
            dept_data_set <- read.csv(dept_file_data$datapath,
                                      header = input$header, sep = " ")
            dept_data_set$V1 <- as.character(dept_data_set$V1)
            dept_data_set$V2 <- as.character(dept_data_set$V2)
            return(dept_data_set)
        }
    )
    
    # Getting the top Nodes that have highest number the email recieved.
    high_sender <- reactive(
        {
            get_email_data <- email_data()
            if(is.null(get_email_data)){
                return(NULL)
            }
            
            send_email_feq <- as.data.frame(sort(table(get_email_data$Sender), decreasing = T))
            colnames(send_email_feq) <- c("Sender_Node", "Numbers of emails")
            return(send_email_feq)
        }
    )
    
    # Getting the top Nodes that have highest number the email recieved.
    high_recieve <- reactive(
        {
            get_email_data <- email_data()
            if(is.null(get_email_data)){
                return(NULL)
            }
            
            recv_email_feq  <- as.data.frame(sort(table(get_email_data$Reciever), decreasing = T))
            colnames(recv_email_feq) <- c("Reciever_Node", "Numbers of emails")
            return(recv_email_feq)
        }
    )
    
    build_graph <- reactive(
        {
            get_email_data <- email_data()
            get_dept_data <- dept_data()
            
            if(is.null(get_email_data)){
                return(NULL)
            }
            
            if(is.null(get_dept_data)){
                return(NULL)
            }
            
            # Getting the list of all unique employees present in the data set.
            unique_emp <- as.data.frame(
                unique(c(get_email_data$Sender , get_email_data$Reciever))
            )
            
            # Now preparing a data frame that can be used as vertex property data-frame
            # while generating iGraph object
            colnames(unique_emp) <- "V1"
            
            # merging the two data frame to map each enployee with it's department.
            unique_emp_dept <- merge(x = unique_emp, y = get_dept_data)
            colnames(unique_emp_dept) <- c("name", "group")
            
            g <- graph.data.frame(d = get_email_data, directed = F, vertices = unique_emp_dept)
            return(g)
        }
    )
    
    degree_table <- reactive(
        {
            i_graph <- build_graph()
            if(is.null(i_graph)){
                return(NULL)
            }
            
            degree_table <- as.data.frame(sort(degree(i_graph), decreasing = T))
            colnames(degree_table) <- c("Degree Centrality")
            degree_table$node_name <- row.names(degree_table)
            degree_table <- degree_table[ ,c("node_name", "Degree Centrality")]
            rownames(degree_table) <- c(1:nrow(degree_table))
            return(degree_table)
        }
    )
    
    between_table <-reactive(
        {
            i_graph <- build_graph()
            if(is.null(i_graph)){
                return(NULL)
            }
            
            between_table <- as.data.frame(sort(betweenness(i_graph, directed = F), decreasing = T))
            colnames(between_table) <- c("Betweeness Centrality")
            between_table$node_name <- row.names(between_table)
            between_table <- between_table[ ,c("node_name", "Betweeness Centrality")]
            rownames(between_table) <- c(1:nrow(between_table))
            return(between_table)
        }
    )
    
    build_graph_d <- reactive(
        {
            get_email_data <- email_data()
            get_dept_data <- dept_data()
            if(is.null(get_email_data)){
                return(NULL)
            }
            
            if(is.null(get_dept_data)){
                return(NULL)
            }
            # Getting the list of all unique employees present in the data set.
            unique_emp <- as.data.frame(
                unique(c(get_email_data$Sender , get_email_data$Reciever))
            )
            
            # Now preparing a data frame that can be used as vertex property data-frame
            # while generating iGraph object
            colnames(unique_emp) <- "V1"
            
            # merging the two data frame to map each enployee with it's department.
            unique_emp_dept <- merge(x = unique_emp, y = get_dept_data)
            colnames(unique_emp_dept) <- c("name", "group")
            
            g <- graph.data.frame(d = get_email_data, vertices = unique_emp_dept)
            return(g) 
        }
    )
    
    indegree_table <- reactive(
        {
            i_graph <- build_graph_d()
            if(is.null(i_graph)){
                return(NULL)
            }
            
            indegree_table <- as.data.frame(sort(degree(i_graph, mode = "in"), decreasing = T))
            colnames(indegree_table) <- c("InDegree Centrality")
            indegree_table$node_name <- row.names(indegree_table)
            indegree_table <- indegree_table[ ,c("node_name", "InDegree Centrality")]
            rownames(indegree_table) <- c(1:nrow(indegree_table))
            return(indegree_table)
        }
    )
    
    dept_table <- reactive(
        {
            get_email_data <- email_data()
            get_dept_data <- dept_data()
            
            if(is.null(get_email_data)){
                return(NULL)
            }
            
            if(is.null(get_dept_data)){
                return(NULL)
            }
            
            merged_data <- merge.data.frame(x = get_email_data, y = get_dept_data, 
                                            by.x = names(get_email_data)[1], by.y = names(get_dept_data)[1],
                                            all.x = T, sort = F)
            colnames(merged_data) <- c("Emp1", "Emp2", "Dept")
            
            final_data <- merge.data.frame(x = merged_data, y = get_dept_data, all.x = T, sort = F,
                                           by.x = names(merged_data)[2], by.y = names(get_dept_data)[1])
            
            colnames(final_data) <- c("Emp2", "Emp1", "Dept_send", "Dept_recv")
            email_dept_data <- final_data[ , c("Dept_send", "Dept_recv")]
            dept_email_freq <- count(df = email_dept_data,vars = c("Dept_send", "Dept_recv"))
            return(dept_email_freq)
        }
    )
    
    build_graph_dept <- reactive(
        {
            dept_email <- dept_table()
            if(is.null(dept_email)){
                return(NULL)
            }
            
            g <- graph.data.frame(d = dept_email)
            return(g)
        }
    )
    
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #                                   All the Render function blocks of the Shiny App                                 #
    #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    # Rendering the DataTable showing 'N' email connection.
    output$nconnectionemail <- renderDataTable(
        datatable(
            {
                get_email_data <- email_data()
                return(get_email_data)
            },
            options = list(searching = FALSE)
        )
    ) 
    
    # Rendering the DataTable showing 'N' rows of connection.
    output$ndepartment <- renderDataTable(
        datatable(
            {
                get_dept_data <- dept_data()
                return(get_dept_data)
            },
            options = list(searching = FALSE)
        )    
    )
    
    # Showing the "N" connectiond via networkD3
    output$nconnectionsgraph <- renderSimpleNetwork(
        {
            get_email_data <- email_data()
            if(!is.null(get_email_data))
                simpleNetwork(get_email_data, charge = -5)
        }
    )
    
    # getting the Nodes that sends the highest number of emails.
    output$highestsender <- renderDataTable(
        datatable(
            {high_sender()},
            options = list(searching = FALSE)
        )
    ) 
    
    # Gettting the Nodes that recieves the max email.
    output$highestreciever <- renderDataTable(
        datatable(
            { high_recieve()},
            options = list(searching = FALSE)
        )
    )
    
    # Getting the graph showing Upto 2-hop neighbours for the top 10 senders.
    output$hopneighboursender <- renderVisNetwork(
        {
            sender_data <- high_sender()
            get_email_data <- email_data()
            
            i_graph <- graph.data.frame(get_email_data, directed = F)
            if(length(high_sender()$node_name < 10)){
                node_list <- high_sender()$node_name[1:length(high_sender()$node_name)]
            }else {
                node_list <- sender_data$Sender_Node[1:10]
            }
            
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_nicely") %>%
                visNodes(size = 50,   height = "500px", width = "100%",
                         color = list( highlight = "blue", hover= "pink")) %>%
                visInteraction(keyboard = TRUE)%>% 
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
                           nodesIdSelection = list(enabled = T, values = node_list,
                                                   selected = node_list[1])) %>%
                visInteraction(keyboard = TRUE)
        }
    )
    
    # Getting the graph showing Upto 2-hop neighbours for the top 10 reciever.
    output$hopneighboursrecv <- renderVisNetwork(
        {
            sender_data <- high_recieve()
            get_email_data <- email_data()
            
            i_graph <- graph.data.frame(get_email_data, directed = F)
            if(length(high_recieve()$node_name < 10)){
                node_list <- high_recieve()$node_name[1:length(high_recieve()$node_name)]
            }else {
                node_list <- sender_data$Reciever_Node[1:10]
            }
            
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_nicely") %>%
                visNodes(size = 50,   height = "500px", width = "100%",
                         color = list( highlight = "blue", hover= "pink")) %>%
                visInteraction(keyboard = TRUE)%>% 
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
                           nodesIdSelection = list(enabled = T, values = node_list,
                                                   selected = node_list[1])) %>%
                visInteraction(keyboard = TRUE)
        }
    )
    
    # getting the Nodes that has the highest degree centrality.
    output$highestdegree <- renderDataTable(
        datatable(
            {degree_table()},
            options = list(searching = FALSE)
        )
    )
    
    # Creating the two hop of the top 10 dergree centrality
    output$hopneighbordegree <- renderVisNetwork(
        {
            i_graph <- build_graph()
            if(length(degree_table()$node_name < 10)){
                node_list <- degree_table()$node_name[1:length(degree_table()$node_name)]
            }else{
                node_list <-  degree_table()$node_name[1:10]
            }
            
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_nicely") %>%
                visNodes(size = 50,   height = "500px", width = "100%",
                         color = list( highlight = "black", hover= "pink")) %>%
                visInteraction(keyboard = TRUE)%>% 
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
                           nodesIdSelection = list(enabled = T, values = node_list,
                                                   selected = node_list[1])) %>%
                visInteraction(keyboard = TRUE)
        }
    )
    
    # getting the Nodes that has the highest Betweeness centrality.
    output$highestbetween <- renderDataTable(
        datatable(
            {between_table()},
            options = list(searching = FALSE)
        )
    )
    
    # Creating the two hop of the top 10 Betweeness centrality
    output$hopneighborbetween <- renderVisNetwork(
        {
            i_graph <- build_graph()
            if(length(between_table()$node_name < 10)){
                node_list <- between_table()$node_name[1:length(between_table()$node_name)]
            }else {
                node_list <-  between_table()$node_name[1:10]
            }
            
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_nicely") %>%
                visNodes(size = 50,   height = "500px", width = "100%",
                         color = list( highlight = "black", hover= "pink")) %>%
                visInteraction(keyboard = TRUE)%>% 
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
                           nodesIdSelection = list(enabled = T, values = node_list,
                                                   selected = node_list[1])) %>%
                visInteraction(keyboard = TRUE)
        }
    )
    
    output$highestindegree <- renderDataTable(
        datatable(
            {indegree_table()},
            options = list(searching = FALSE)
        )
    )
    
    output$hopneighborindegree <- renderVisNetwork(
        {
            i_graph <- build_graph_d()
            arpit <<- indegree_table()$node_name
            if(length(indegree_table()$node_name < 10)){
                node_list <- indegree_table()$node_name[1:length(indegree_table()$node_name)]
            } else {
                node_list <- indegree_table()$node_name[1:10]
            }
            
            #print(node_list)
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_nicely") %>%
                visNodes(size = 50,   height = "500px", width = "100%",
                         color = list( highlight = "black", hover= "pink")) %>%
                visInteraction(keyboard = TRUE)%>% 
                visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T), 
                           nodesIdSelection = list(enabled = T, values = node_list,
                                                   selected = node_list[1])) %>%
                visInteraction(keyboard = TRUE)
        }
    )
    
    output$observation <- renderText(
        {
            if(is.null(email_data()))
            {return(NULL)}
            
            "In all the three centralities (when all the observation of the file is considered) node '160' 
            has the heigest number of Degree, Betweeness and In-Degree centrality.
            It can be concluded that this node in the most important node on this network on the basis of its 
            Degree, Betweeness or In-Degree centrality value."
        }
    )
    output$deptemailtable <- renderDataTable(
        datatable(
            {dept_table()},
            options = list(seaching = FALSE)
        )
    )
    
    output$deptemailgraph <-renderVisNetwork(
        {
            i_graph <- build_graph_dept()
            
            visIgraph(i_graph) %>%
                visIgraphLayout(i_graph, layout = "layout_in_circle") %>%
                visNodes(size = 5,   height = "500px", width = "100%",
                         color = list(hover= "pink"),
                         font = list(size = 50)) 
        }
    )
        }

shinyApp(ui = ui, server = server)

