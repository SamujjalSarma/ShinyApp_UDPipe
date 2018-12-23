#Max file size is of 30 MB
options(shiny.maxRequestSize=30*1024^2) 
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
    if (is.null(input$file1)) {
      return(print("Upload proper .txt file"))
    }
    if (is.null(input$file2)) {
      return(print("Upload proper udpipe Model"))
    }
    else{
    language_model = udpipe_load_model(file=input$file2$datapath)
    #English Model
    if(input$file2$name =='english-ud-2.0-170801.udpipe'){
    data2 <- as.character(Dataset())

    x <- udpipe_annotate(language_model, x = data2)
    x <- as.data.frame(x)
    x <- subset(x,x$xpos %in% input$XPOS)
    }
    #Spanish MOdel XPOS to UPOS
    else if(input$file2$name =='spanish-ud-2.0-170801.udpipe'){
      input_spanish<- c()
      for (i in seq_along(input$XPOS)){

        if (input$XPOS[i]=='JJ'){
          input_spanish[[i]]='ADJ'
        }
        else if(input$XPOS[i]=='NN'){
          input_spanish[[i]]='NOUN'
        }
        else if(input$XPOS[i]=='NNP'){
          input_spanish[[i]]='PORPN'
        }
        else if(input$XPOS[i]=='VB'){
          input_spanish[[i]]='VERB'
        }
        else if(input$XPOS[i]=='RB'){
          input_spanish[[i]]='ADV'
        }
        else{}
      }
      data2 <- as.character(Dataset())
      x <- udpipe_annotate(language_model, x = data2)
      x <- as.data.frame(x)
      x <- subset(x,x$upos %in% input_spanish)
      
    }
    else if(input$file2$name =='hindi-ud-2.0-170801.udpipe'){
      windowsFonts(devanew=windowsFont("Devanagari new normal"))
      data2 <- as.character(Dataset())
      x <- udpipe_annotate(language_model, x = data2)
      x <- as.data.frame(x)
      x <- subset(x,x$xpos %in% input$XPOS)
    }
    }
  })
  
  #Take xpos selection
  XPOS <- reactive({
    (input$XPOS)
  })
  

  
  output$plot2 = renderPlot({ 
    
    if (is.null(input$file1)) {
      return(print("Upload proper .txt file"))
    }
    if (is.null(input$file2)) {
      return(print("Upload proper udpipe Model"))
    }
    else{
    
    language_model = udpipe_load_model(file=input$file2$datapath)
    data2 <- as.character(Dataset())
    
    if(input$file2$name =='english-ud-2.0-170801.udpipe'){
    x <- udpipe_annotate(language_model, x = data2)
    x <- as.data.frame(x)
    cooc <- cooccurrence( x <- subset(x, x$xpos %in% input$XPOS),
                          term = "lemma", 
                          group  = c("doc_id", "paragraph_id", "sentence_id")
    )  
    }
    #Spanish MOdel XPOS to UPOS
    else if(input$file2$name =='spanish-ud-2.0-170801.udpipe'){
      input_spanish<- c()
      for (i in seq_along(input$XPOS)){
        
        if (input$XPOS[i]=='JJ'){
          input_spanish[[i]]='ADJ'
        }
        else if(input$XPOS[i]=='NN'){
          input_spanish[[i]]='NOUN'
        }
        else if(input$XPOS[i]=='NNP'){
          input_spanish[[i]]='PORPN'
        }
        else if(input$XPOS[i]=='VB'){
          input_spanish[[i]]='VERB'
        }
        else if(input$XPOS[i]=='RB'){
          input_spanish[[i]]='ADV'
        }
        else{}
      }
      data2 <- as.character(Dataset())
      x <- udpipe_annotate(language_model, x = data2)
      x <- as.data.frame(x)
      
      cooc <- cooccurrence( x <- subset(x,x$upos %in% input_spanish),
                            term = "lemma", 
                            group  = c("doc_id", "paragraph_id", "sentence_id"))
    }
    else if(input$file2$name =='hindi-ud-2.0-170801.udpipe'){
      data2 <- as.character(Dataset())
      x <- udpipe_annotate(language_model, x = data2)
      x <- as.data.frame(x)
      windowsFonts(devanew=windowsFont("Devanagari new normal"))
      cooc <- cooccurrence( x <- subset(x, x$xpos %in% input$XPOS),
                            term = "lemma", 
                            group  = c("doc_id", "paragraph_id", "sentence_id"))
    }
    wordnetwork <- head(cooc, 50)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance") 
    }
    
  })
  
   output$wordcloud = renderPlot({
     if (is.null(input$file1)) {
       return(print("Upload proper .txt file"))
     }
     if (is.null(input$file2)) {
       return(print("Upload proper udpipe Model"))
     }
     else{
 
  
  language_model = udpipe_load_model(file=input$file2$datapath)
  data2 <- as.character(Dataset())
  
  if(input$file2$name =='english-ud-2.0-170801.udpipe'){
    x <- udpipe_annotate(language_model, x = data2)
    x <- as.data.frame(x)
   
     all_nouns = x %>% subset(., x$upos %in% "NOUN"); all_nouns$token[1:20]
    top_nouns = txt_freq(all_nouns$lemma)
    head(top_nouns$key, 20) 
    
    all_verbs = x %>% subset(., x$upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    head(top_verbs$key, 20)
  }
  #Spanish MOdel XPOS to UPOS
  else if(input$file2$name =='spanish-ud-2.0-170801.udpipe'){
    
    data2 <- as.character(Dataset())
    x <- udpipe_annotate(language_model, x = data2)
    x <- as.data.frame(x)
    
    all_nouns = x %>% subset(., x$upos %in% "NOUN"); all_nouns$token[1:20]
    top_nouns = txt_freq(all_nouns$lemma)
    head(top_nouns$key, 20) 
    
    all_verbs = x %>% subset(., x$upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    head(top_verbs$key, 20)
  }
  else if(input$file2$name =='hindi-ud-2.0-170801.udpipe'){
    data2 <- as.character(Dataset())
    x <- udpipe_annotate(language_model, x = data2)
    x <- as.data.frame(x)
    windowsFonts(devanew=windowsFont("Devanagari new normal"))
    
    all_nouns = x %>% subset(., x$upos %in% "NOUN"); all_nouns$token[1:20]
    top_nouns = txt_freq(all_nouns$lemma)
    head(top_nouns$key, 20) 
    
    all_verbs = x %>% subset(., x$upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    head(top_verbs$key, 20)
  }
  
  wordcloud(words = top_nouns$key, 
            freq = top_nouns$freq, 
            min.freq = 2, 
            max.words = 100,
            random.order = FALSE, 
            colors = brewer.pal(6, "Dark2"))
     }
   })
  
})


