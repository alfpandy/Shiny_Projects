####################################################################################################
## A Shiny app to test children's knowledge on the times table ####################################
####################################################################################################

#type in "shinyapp" and click shift+tab
library(shiny)
library(ggplot2)
library(shinyjs)

ui <- fluidPage(
   useShinyjs(),  # to reset "my_answer" textOutput
  # Application title
  titlePanel("Multiplication Test"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(input="tt_value", label="Select your times table", choices = c("Any", seq(2,12)), selected = "Any"),
      actionButton("goagain", "Ask a question?"),
      br(),
      br(),
      br(),
      textInput(input = "my_answer", label = "Type in your answer and press \" Go\"", placeholder = "your answer"), 
      br(),
     
      actionButton("goButton", "Give my answer")
        
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("question"),
      br(),
      textOutput("answer"),
      br(),
      textOutput("cumcount")
    )
  )
)

#ui <- fluidPage(
  ###### input widgets (controls)
  ##### widget("id", label, listofvalues/choices)
  
  ####### output controls
  ####   plotOutput("name")
#)

server <- function(input, output, session) {
   
   #eventReactive if variable is to change each time a button is pressed
   #reactive if variable stays constant each time a button is depressed.
   times_table_val <- eventReactive(
      input$goagain,
      {if(input$tt_value == "Any"){
         sample(2:12, 1) 
      } else
      {input$tt_value}
         }
   )
   
   generated_var <- eventReactive(
     input$goagain, {sample(1:12, 1)}
   )


   output$question <- renderText(
     paste("What is ", times_table_val(), " times ", {generated_var()}, " ? ")
   )

   ans <- reactive(
     generated_var()*as.numeric(times_table_val())
   )

   #reactive value variable - will change value according to rule
   rv <- reactiveValues(count = 0)

   # counts correct answers and prints whether answer is correct
   # ans_compare <- eventReactive(input$goButton, {
   #    if (as.numeric(input$my_answer) != as.numeric(ans())){
   #       print("Not quite right. Have another go")
   #    } else {
   #       a <- c("Fantastic answer! You are correct")
   #       b <- c("Well done! You are correct")
   #       c <- c("Great answer!")
   #       d <- c("You are really good at this!")
   #       e <- c("You are the best at maths")
   #       rv$count <- rv$count +1
   #       sample(c(a,b,c,d,e),1)
   #    })
   
   # 
   ans_compare <- eventReactive(input$goButton, {
     a <- c("Fantastic answer! You are correct")
     b <- c("Well done! You are correct")
     c <- c("Great answer!")
     d <- c("You are really good at this!")
     e <- c("You are the best at maths")
     if (as.numeric(input$my_answer) == as.numeric(ans())){
       rv$count <- rv$count +1
       sample(c(a,b,c,d,e),1)
     } else {
       print("Not quite right. Have another go")
     }

   }
   )

   #resets numberInput panel once "my_answer" is submitted 
   observeEvent(input$goButton, {
      reset("my_answer")    # part of shinyjs package, initiated by useShinyjs()
   })

   output$answer <- renderText(
     ans_compare()
   )

   output$cumcount <- renderText(
     paste("You have a total of ", rv$count, " correct answers!")
   )
   
}

shinyApp(ui, server)