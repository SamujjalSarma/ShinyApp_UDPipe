#Max file size is of 30 MB
options(shiny.maxRequestSize=30*1024^2) 
if (!require(udpipe)){install.packages("udpipe")}
if (!require(lattice)){install.packages("lattice")}
if (!require(textrank)){install.packages("textrank")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
#Starts with the ShinyServer
shinyServer(function(input, output) {
  Dataset <- reactive({
    
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return(NULL) } else{
        
        Data <- readLines(input$file1$datapath)
        return(Data)
      }
  })
  
  udpipe_model <- reactive({
    if (is.null(input$file2)) {   # locate 'file2' from ui.R
      
      return(NULL) } else{
        
        udpipe_model <- input$file2$datapath
        return(udpipe_model)
      }
  })
  
  
  # Take input .txt file
  output$inputfile_annotated = renderTable({ 
    
    english_model = udpipe_load_model(file=input$file2$datapath)
    data2 <- as.character(Dataset())
    x <- udpipe_annotate(english_model, x = data2,doc_id = seq_along(data2))
    x <- as.data.frame(x)
    x <- subset(x,x$xpos %in% input$XPOS)
    
    
  })
  
  #Take xpos selection
  XPOS <- reactive({
    (input$XPOS)
  })
  
  output$clust_summary = renderTable({
    english_model = udpipe_load_model(file=input$file2$datapath)
    
    data2 <- as.character(Dataset())
    x <- udpipe_annotate(english_model, x = data2,doc_id = seq_along(data2))
    
    x <- as.data.frame(x)
    cooc <- cooccurrence( x <- subset(x, x$xpos %in% input$XPOS),
      term = "lemma", 
      group  = c("doc_id", "paragraph_id", "sentence_id")
      )  
     cooc
  })
  
  output$plot2 = renderPlot({ 
    
    english_model = udpipe_load_model(file=input$file2$datapath)
    data2 <- as.character(Dataset())
    x <- udpipe_annotate(english_model, x = data2,doc_id = seq_along(data2))
    x <- as.data.frame(x)
    cooc <- cooccurrence( x <- subset(x, x$xpos %in% input$XPOS),
                          term = "lemma", 
                          group  = c("doc_id", "paragraph_id", "sentence_id")
    )  
    
    wordnetwork <- head(cooc, 50)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance") 
    
  })
  
})


