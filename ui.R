#---------------------------------------------------------------------#
#               UDPipe NLP Workflow                                   #
#---------------------------------------------------------------------#


library("shiny")

shinyUI(
  fluidPage(
    
    titlePanel("UDPipe NLP Workflow"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file1", "Upload data (txt file)"),
        fileInput("file2", "Upload UDPipe Model (Upload for any languages)"),
        
        checkboxGroupInput("XPOS", "select list of part-of-speech tags (XPOS):",
                           c("adjective (JJ)" = "JJ",
                             "noun(NN)" = "NN",
                             "proper noun (NNP)" = "NNP",
                             "adverb (RB)" = "RB",
                             "verb (VB)" = "VB"),
                           selected = list("JJ","NN","NNP"),)),
         
      
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app supports only txt format data file. .txt format file should have text content in it",align="justify"),
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (txt file).")),
                               span(strong("And upload UDPipe Model (Model for any language).")),
                               'POS tags check boxes selection helps build Annotated table accordingly in the first tab, Co-occurance plot in the second tab and word cloud is present in the third tab with Nouns and Verbs of the text uploaded through .txt file ')),
                   
                    tabPanel("Annotated document", 
                             tableOutput('inputfile_annotated')),
                    
                    tabPanel("Co-occurrences plot",
                             plotOutput('plot2')),
                    
                    tabPanel("Word Cloud",
                             plotOutput('wordcloud'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI




