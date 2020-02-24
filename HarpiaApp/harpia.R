library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(hash)
library(stringr)
library(lmerTest)
require(ggplot2)
library(markovchain)
library(igraph)
library(mixOmics)
library(Boruta)
library(gtools)
library(stringi)
library(readr)
library(ggedit)
library(plotly)
library(devtools)
library(reticulate)



source("getAlphabets.R")
source("getDataStructure.R")
source("linearModel.R")
source("MarkovModel.R")
source("JS.R")


jscode <- "shinyjs.refresh = function() { history.go(0); }"
ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = 'Harpia'),
                    
                    
                    dashboardSidebar(
                      shinyjs::useShinyjs(),
                      sidebarMenu(id = "mySidebar",
                                  menuItem("Folder Uploads", tabName = "folder", icon = icon("far fa-folder")),
                                  menuItem("Alphabet Upload",tabName = "file", icon = icon("far fa-file")),
                                  menuItem("Entropy level", tabName = "entropy", icon = icon("far fa-bar-chart")),
                                  menuItem("Pseudo count", tabName = "pc", icon = icon("far fa-plus"))
                      ),
                      
                      box(title = "Run Harpia!", solidHeader = T, width = 12,collapsible = TRUE,background = "black",align = "left",
                          actionButton("run", icon(name = "fas fa-play", class = "fa 4x"), width = '85%'
                          ))
                      
                    ),
                    
                    
                    dashboardBody(
                      useShinyjs(),
                      extendShinyjs(text = jscode),
                      conditionalPanel(
                        condition = ("input.run == 0"),
                        
                        tags$p(),
                        
                        
                        tabItems(
                          tabItem(tabName = "folder",
                                  h3("Folder upload"),
                                  helpText("Please upload folders containg tab delimited files."),
                                  box(title = "Folder 1", solidHeader = T,
                                      fluidRow(
                                        column(2, offset = 0, 
                                               shinyDirButton('folder_G1', 'Group 1', 'Please select a folder')),
                                        
                                        column(2, offset = 3, 
                                               checkboxInput("labelcheck", "Label"))),
                                      conditionalPanel(
                                        condition = "input.labelcheck == true",
                                        textInput("label1","Label for Group 1:","")
                                      ),htmlOutput("directorypath")),tags$p(),
                                  box(title = "Folder 2", solidHeader = T,
                                      fluidRow(
                                        column(2, offset = 0, 
                                               shinyDirButton('folder_G2', 'Group 2', 'Please select a folder')),
                                        
                                        column(2, offset = 3, 
                                               checkboxInput("label2check", "Label"))),
                                      conditionalPanel(
                                        condition = "input.label2check == true",
                                        textInput("label2","Label for Group 2:","")
                                      ),htmlOutput("directorypath2"))
                                  
                          ),
                          
                          tabItem(tabName = "file",
                                  box(title = "Alphabet file upload", solidHeader = T, width = 12,
                                      fileInput("fileAlphabet", "Choose Alphabet File", accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")))), 
                          
                          tabItem(tabName = "entropy",
                                  
                                  
                                  box(  solidHeader = T, width = 12, 
                                        
                                        selectInput("selectH", label = h4("Select Entropy Level for Analysis"),
                                                    choices = list("-" = 0, "H2" = 2, "H3" = 3, "H4" = 4)))
                          ),
                          tabItem(tabName = "pc",
                                  
                                  box( solidHeader = T, width = 12, 
                                       selectInput("pseudocount", label = h4("Select Pseudocount value for Analysis"),
                                                   choices = list( "1/N" = "pc","1" = 1, "0.5" = 0.5, "0" = 0)))
                          )
                        )
                      )
                      
                      ,
                      conditionalPanel(
                        condition = "input.run",
                        
                        fluidRow(
                          headerPanel("Your Harpia Results"),
                          fluidRow(
                            
                            downloadButton("download", "Download"),
                            
                            actionButton("refresh", "Refresh",icon(name = "refresh", class = "fa 4x fa-spin"))),tags$style(type='text/css', "#download {margin-left: 15px;}"),
                          tabsetPanel(id = "tabset",
                                      tabPanel("Entropy Analysis"
                                               ,plotOutput("plot1"), tags$hr()
                                      ),
                                      tabPanel("Markov Model Graphs", tags$div(class="header", checked=NA, tags$em(bsButton("help2","Info", icon = NULL, style = "inverse",size = "small", type = "action", block = FALSE, disabled = FALSE, value = FALSE))),fluidRow(
                                        box(plotOutput("plot5"))
                                        , box(plotOutput("plot6"))),fluidRow(box(imageOutput("JS_plot"))
                                        )
                                      ),
                                      
                                      
                                      tabPanel("Linear Model Analysis", tags$div(class="header", checked=NA,
                                                                                 
                                                                                 tags$em(bsButton("help1","Info", icon = NULL, style = "inverse",
                                                                                                  size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                                  value = FALSE)
                                                                                 )
                                      ),verbatimTextOutput("summaryMLE")
                                      ,tags$hr(),
                                      fluidRow(box(plotOutput("plot3")
                                      ),box(plotOutput("plot4")
                                      )
                                      ), plotOutput("plot2")
                                      ,tags$hr()
                                      
                                      ), 
                                      
                                      
                                      
                                      
                                      ### Boruta
                                      tabPanel("Boruta - Random forest", tags$div(class="header", checked=NA,
                                                                                  
                                                                                  tags$em(bsButton("help4","Info", icon = NULL, style = "inverse",
                                                                                                   size = "small", type = "action", block = FALSE, disabled = FALSE,
                                                                                                   value = FALSE)
                                                                                  )
                                      ),selectInput("selectB", label = h4("Select Entropy Level for Classification", bsTooltip("selectB", "The entropy level of the analysis should be chosen based on the linear model result found on the Linear Model Analysis tab.",                                                                                                                                     placement = "right", trigger = "hover")),
                                                    choices = list("-" = 0, "H1" = 1, "H2" = 2, "H3" = 3, "H4" = 4)),plotOutput("borutaplot")
                                      ,tags$hr()
                                      ,tags$hr(),tableOutput("bStats")
                                      )
                                      
                                      
                                      
                          )
                          
                        )
                      )
                    ))

server <- shinyServer(function(input, output, session) {
  start_time <- Sys.time()
  observeEvent(input$run,addClass(selector = "body", class = "sidebar-collapse"))
  
  
  volumes = getVolumes()
  
  
  folderInput1 = NULL
  folderInput1 <- reactive({
    shinyDirChoose(input, 'folder_G1', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$folder_G1))
  })
  folderInput2 = NULL
  folderInput2 <- reactive({
    shinyDirChoose(input, 'folder_G2', roots = volumes, session = session, 
                   restrictions = system.file(package = 'base'))
    return(parseDirPath(volumes, input$folder_G2))
  })
  
  
  output$directorypath <- renderUI({
    HTML(folderInput1())
  })
  output$directorypath2 = renderUI({
    HTML(folderInput2())
  })
  
  
  observeEvent(input$run,{
    
    
    withProgress(
      {
        alphabet = input$fileAlphabet
        alphabets = getAlphabetsR(alphabet$datapath)
        #Generate alphabets for all possible entropy levels
        alphabetH1 = unlist(alphabets$H1)
        alphabetH2 = unlist(alphabets$H2)
        alphabetH3 = unlist(alphabets$H3)
        alphabetH4 = unlist(alphabets$H4)
        #Alphabet Size
        nH1 = length(alphabetH1)
        nH2 = length(alphabetH2)
        nH3 = length(alphabetH3)
        nH4 = length(alphabetH4)}
      ,
      message = 'Creating Alphabets...',
      detail = ''
    )
    
    
    files1 <- reactive({
      list.files(path = folderInput1(), pattern = "*.csv", full.names = T)
    })
    nFiles1 <-reactive({length(files1())})
    
    files2 <- reactive({
      list.files(path = folderInput2(), pattern = "*.csv", full.names = T)
    })
    nFiles2 <- reactive({ length(files2())})
    
    f1 = isolate(files1())
    print(folderInput1())
    f2 = isolate(files2())
    print(f2)
    
    updateSelectInput(session, 'selectB', choices =c(1:input$selectH))
    
    if(input$selectH == 2){
      
      EntropyAnalysisGroup1 = reactive({if(!is.null(f1)){
        lof1 =  isolate(files1())
        path1 = isolate(folderInput1())
        nf1 = isolate(nFiles1())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc = 1/nH2
        }
        
        print(pc)
        combo = getDataR_upto2(as.character(path1),alphabetH1,alphabetH2, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
      EntropyAnalysisGroup2 = reactive({if(!is.null(f2)){
        lof2 =  isolate(files2())
        path2 = isolate(folderInput2())
        nf2 = isolate(nFiles2())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc = 1/nH2
        }
        
        
        print(pc)
        combo = getDataR_upto2(as.character(path2),alphabetH1,alphabetH2, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
    }
    
    if(input$selectH == 3){
      
      EntropyAnalysisGroup1 = reactive({if(!is.null(f1)){
        lof1 =  isolate(files1())
        path1 = isolate(folderInput1())
        nf1 = isolate(nFiles1())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc = 1/nH3
        }
        
        
        print(pc)
        combo = getDataR_upto3(as.character(path1),alphabetH1,alphabetH2,alphabetH3, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
      EntropyAnalysisGroup2 = reactive({if(!is.null(f2)){
        lof2 =  isolate(files2())
        path2 = isolate(folderInput2())
        nf2 = isolate(nFiles2())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc = 1/nH3
        }
        
        
        print(pc)
        combo = getDataR_upto3(as.character(path2),alphabetH1,alphabetH2,alphabetH3, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
    }
    
    
    if(input$selectH == 4){
      
      EntropyAnalysisGroup1 = reactive({if(!is.null(f1)){
        lof1 =  isolate(files1())
        path1 = isolate(folderInput1())
        nf1 = isolate(nFiles1())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc =1/nH4
        }
        
        combo = getDataR_upto4(as.character(path1),alphabetH1,alphabetH2,alphabetH3, alphabetH4, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
      EntropyAnalysisGroup2 = reactive({if(!is.null(f2)){
        lof2 =  isolate(files2())
        path2 = isolate(folderInput2())
        nf2 = isolate(nFiles2())
        pc = input$pseudocount
        
        if(pc == "pc"){
          pc = 1/nH4
        }
        
        
        combo = getDataR_upto4(as.character(path2),alphabetH1,alphabetH2,alphabetH3,alphabetH4, pc)
        
        return(combo)}
        else(NULL)
        
        
      })
      
    }
    
    withProgress(message = "Calculating Entropy", value = 0.1, {
      
      
      Group1Data = EntropyAnalysisGroup1()
      incProgress(1/4, detail = paste(": Folder 1"))
      Group2Data = EntropyAnalysisGroup2()
      incProgress(2/4, detail = paste(": Folder 2"))
      end_time <- Sys.time()
      print(end_time - start_time)
      
      #####$ Analysis: 
      incProgress(3/4, detail = paste("Generating plot"))
      plot1 = reactive({
        if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1())){
          # Group2_Data = EntropyAnalysisGroup2()
          # Group1_Data = EntropyAnalysisGroup1()
          # 
          EntropyGroup1 = Group1Data$tEntropy
          EntropyGroup2 = Group2Data$tEntropy

          if(input$selectH == 2){
            EntropyGroup1LM = as.matrix(cbind(EntropyGroup1$H0,EntropyGroup1$H1,EntropyGroup1$H2))
            
            EntropyGroup2LM = as.matrix(cbind(EntropyGroup2$H0,EntropyGroup2$H1,EntropyGroup2$H2))
            
          }
          if(input$selectH == 3){
            EntropyGroup1LM = as.matrix(cbind(EntropyGroup1$H0,EntropyGroup1$H1,EntropyGroup1$H2,EntropyGroup1$H3))
            
            EntropyGroup2LM = as.matrix(cbind(EntropyGroup2$H0,EntropyGroup2$H1,EntropyGroup2$H2,EntropyGroup2$H3))
            
          }
          if(input$selectH == 4){
            EntropyGroup1LM = as.matrix(cbind(EntropyGroup1$H0,EntropyGroup1$H1,EntropyGroup1$H2,EntropyGroup1$H3,EntropyGroup1$H4 ))
            
            EntropyGroup2LM = as.matrix(cbind(EntropyGroup2$H0,EntropyGroup2$H1,EntropyGroup2$H2,EntropyGroup2$H3,EntropyGroup2$H4))
            
            
          }
          colnames(EntropyGroup1LM) =  Group1Data$levels
          colnames(EntropyGroup2LM) =  Group2Data$levels

          Group2 = colMeans(EntropyGroup2LM)
          
          Group1 = colMeans(EntropyGroup1LM)
          
          statsG1 = c()
          statsG2 = c()
          for (level in colnames(EntropyGroup2LM)){
            statsG2 = c(statsG2,as.numeric(sd(EntropyGroup2LM[,level])))
            statsG1 = c(statsG1,as.numeric(sd(EntropyGroup1LM[,level])))
          }
          G2Quantiles = t(matrix(statsG2, ncol=length(colnames(EntropyGroup2LM)), byrow=TRUE))
          G2Quantiles = as.data.frame(G2Quantiles, stringsAsFactors=FALSE)
          colnames(G2Quantiles) = c("SD")
          row.names(G2Quantiles) = colnames(EntropyGroup2LM)
          
          G1Quantiles = t(matrix(statsG1, ncol=length(colnames(EntropyGroup1LM)), byrow=TRUE))
          G1Quantiles = as.data.frame(G1Quantiles, stringsAsFactors=FALSE)
          colnames(G1Quantiles) = c("SD")
          row.names(G1Quantiles) = colnames(EntropyGroup1LM)
          
          
          dataEntropy = data.frame(
            Group = factor(c(rep("Group2",length(colnames(EntropyGroup2LM))),c(rep("Group1",length(colnames(EntropyGroup2LM)))))),
            Level = factor(c(rep(colnames(EntropyGroup2LM),2)), levels=c(colnames(EntropyGroup2LM))),
            Entropy = c(Group2,Group1),
            SD = c(G2Quantiles$SD,G1Quantiles$SD)
          )
          
          
          pd <- position_dodge(0.05) 
          # 
          
          if(input$label1 != ""){
            label1 = input$label1
          }
          else{label1 = "Group1" }
          if(input$label2 != ""){
            label2 = input$label2
          }
          else{label2 = "Group2" }
          
          return({ggplot(data=dataEntropy, aes(x=Level, y=Entropy, group=Group, colour= Group)) +
              geom_errorbar(aes(ymin=Entropy-SD, ymax=Entropy+SD), width=.1,position=pd) +
              geom_line() +
              scale_color_manual(labels=c(label1,label2),values=c('firebrick3','deepskyblue3'))+
              geom_point(position=pd, size=3)
            
          })
        }
        
      })
      output$plot1 = renderPlot({ 
        plot1()
      })
      
    })
    
    withProgress({
      lmerAnalysis = reactive({
        if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1())){
          
          Group2_Data = EntropyAnalysisGroup2()
          Group1_Data = EntropyAnalysisGroup1()
          if(input$label1 != ""){
            label1 = input$label1
          }
          else{label1 = "Group1" }
          if(input$label2 != ""){
            label2 = input$label2
          }
          else{label2 = "Group2" }
          LM1 = linearModelR(Group1_Data$levels, Group1_Data$Entropy, label1)
          LM2 = linearModelR(Group2_Data$levels, Group2_Data$Entropy, label2)
          
          G1_LM = (LM1$LM)
          #G1_LM = lapply(G1_LM[,2], as.numeric)
          #print(unlist(LM1))
          G2_LM = (LM2$LM)
          #G2_LM = lapply(G2_LM[,2], as.numeric)
          #print(LM2)
          MLEData = rbind(G1_LM,G2_LM)
          mod1 = lmer(Entropy ~ Genotype*Level +  (1|Mouse),MLEData)
          summary(mod1)
          combo = list(mod1 = mod1, MLEData = MLEData)
          return(combo)
          
        }
        else(return(NULL))
        
      })
      
      plot2 = reactive({    
        if(!is.null(lmerAnalysis())){
          outputOptions(output,"plot2",suspendWhenHidden=FALSE)
          MLEData = lmerAnalysis()
          MLEData = MLEData$MLEData
          x = interaction(MLEData$Genotype,MLEData$Level)
          if(input$label1 != ""){
            label1 = input$label1
          }
          else{label1 = "Group1" }
          if(input$label2 != ""){
            label2 = input$label2
          }
          else{label2 = "Group2" }
          return({
            ggplot(MLEData, aes(x= x , y=Entropy,  fill= Genotype)) + geom_boxplot() + scale_fill_manual(labels=c(label1,label2),values=c('firebrick3','deepskyblue3')) + labs(x = "Genotype*Level", y = "Entropy")
          })
        }
        
        else(return(NULL))})
      
      output$plot2 = renderPlot({
        if(!is.null(lmerAnalysis())){
          
          print(plot2())}
      })

      
      plot3 = function()({if(!is.null(lmerAnalysis())){
        mle = isolate(lmerAnalysis())
        return({plot(mle$mod1)
        })}})
      
      output$plot3 = renderPlot({
        if(!is.null(lmerAnalysis())){
          plot3()}
      })
      
      
      
      
      plot4 = reactive({if(!is.null(lmerAnalysis())){
        #outputOptions(output,"plot4",suspendWhenHidden=FALSE)
        mle = isolate(lmerAnalysis())
        return({qqnorm(resid(mle$mod1))
          qqline(resid(mle$mod1))
        })}})
      
      output$plot4 = renderPlot({ 
        print(plot4())
        
      })

      summaryMLE = reactive({if(!is.null(lmerAnalysis())){
        mle = isolate(lmerAnalysis())
        return({summary(mle$mod1)})
      }})
      
      output$summaryMLE = renderPrint({ 
        outputOptions(output,"summaryMLE",suspendWhenHidden=FALSE)
        if(!is.null(lmerAnalysis())){
          # mle = isolate(lmerAnalysis())
          str = unlist(strsplit(as.character(summaryMLE()[1]), "\n"))
          return(summaryMLE())}
      })
      

    ## Markov Graph
    
    plotGRAPH5 = reactive({if(!is.null(EntropyAnalysisGroup1())){

      Group1Data = EntropyAnalysisGroup1()
      `%notin%` <- Negate(`%in%`)
      if(input$label1 == ""){
        group = "Group1"
      }
      else{group = input$label1}
      markovdata = markovmodelR(Group1Data$counts2, input$pseudocount)
      print(markovdata$nodeSize)
      g.copy = 0
      g <- graph.adjacency(as.matrix(markovdata$TPM), weighted=TRUE)
      print(g)
      sortedWeights = sort(E(g)$weight, decreasing = T)[1:10]
      sortedWeights
      print(E(g)$weight)
      g.copy <- delete.edges(g, which(E(g)$weight %notin% sortedWeights))
      deg <- degree(g.copy, mode="all")
      V(g.copy)$size = rowMeans(Group1Data$counts1)*.8
      E(g.copy)$arrow.size <- 1.2
      E(g.copy)$edge.color <- "gray80"
      E(g.copy)$width <- E(g.copy)$weight*15
      V(g.copy)$label.cex = .7
      E(g.copy)$arrow.mode = 2
      V(g.copy)$color = "firebrick3"

      main = paste("Transition Graph for", group, sep = " ")
      return(plot(g.copy, main = main, layout=layout_in_circle(g.copy), vertex.label.color= "black",
                  vertex.label.family = "Helvetica", edge.label.font = 2))
    }
      else(return())
    })
    output$plot5 = renderPlot({ 
      if(!is.null(plotGRAPH5())){
        plotGRAPH5()}
      else(NULL)
    })

      
      
    plotGRAPH6 = reactive({if(!is.null(EntropyAnalysisGroup2())){
      `%notin%` <- Negate(`%in%`)
      Group2_Data = EntropyAnalysisGroup2()
      if(input$label2 == ""){
        group = "Group2"
      }
      else{group = input$label2}
      markovdata = markovmodelR(Group2_Data$counts2, input$pseudocount)
      g.copy = 0
      g <- graph.adjacency(as.matrix(markovdata$TPM), weighted=TRUE)
      sortedWeights = sort(E(g)$weight, decreasing = T)[1:10]
      sortedWeights
      print(E(g)$weight)
      g.copy <- delete.edges(g, which(E(g)$weight %notin% sortedWeights))
      deg <- degree(g.copy, mode="all")
      V(g.copy)$size = rowMeans(Group2_Data$counts1)*.8
      E(g.copy)$arrow.size <- 1.2
      E(g.copy)$edge.color <- "gray80"
      E(g.copy)$width <- E(g.copy)$weight*15
      V(g.copy)$label.cex = .7
      E(g.copy)$arrow.mode = 2
      V(g.copy)$color="deepskyblue3"

      main = paste("Transition Graph for", group, sep = " ")
      return(plot(g.copy, main = main, layout=layout_in_circle(g.copy), vertex.label.color= "black",
                  vertex.label.family = "Helvetica", edge.label.font = 2))
    }
      else(return())
    })
    output$plot6 = renderPlot({ 
      if(!is.null(EntropyAnalysisGroup2())){
        plotGRAPH6()}
    })

    ### JS divergence
    
    JS_div = reactive({if(!is.null(EntropyAnalysisGroup1())){
      
      Group1Data = EntropyAnalysisGroup1()
      if(input$label1 == ""){
        group = "Group1"
      }
      else{group = input$label1}
      tpm1 = markovmodelR(Group1Data$counts2, input$pseudocount)
      
      Group2Data = EntropyAnalysisGroup2()
      if(input$label2 == ""){
        group = "Group2"
      }
      else{group = input$label2}
      tpm2 = markovmodelR(Group2Data$counts2, input$pseudocount)
      
      
      JS_df = JS_R(data.frame(tpm1), data.frame(tpm2))
      
      JS = data.frame(JS_df$JS_df)
      JS$call = row.names(data.frame(tpm1$TPM))
      colnames(JS) = c("JS_Divergence", "call")
      newdata <- JS[order(JS$JS_Divergence),]
      
      
      return(ggplot(JS, aes(x = reorder(call, -JS_Divergence), y = JS_Divergence, fill=JS_Divergence)) + geom_bar(stat = "identity")+  scale_fill_distiller(palette = "Spectral")+xlab("Calls")+ylab("Jensen-Shannon Divergence"))
    }
      else(return())
    })
      output$JS_plot = renderPlot({ 
        if(!is.null(EntropyAnalysisGroup2())){
          JS_div()}
      })
      

    boruta = reactive({
      if(!is.null(EntropyAnalysisGroup2()) & !is.null(EntropyAnalysisGroup1()) & input$selectB != 0) {    
        Group2_Data = EntropyAnalysisGroup2()
        Group1_Data = EntropyAnalysisGroup1()
        
        if (input$selectB == 1){
          Group1 = (Group1_Data$F1)
          Group2 = (Group2_Data$F1)
          #print(Group1[['C']])
          data1 = c()
          #print(Group1)
          for(call in alphabetH1){
            #print(as.vector(Group1[[call]]))
            data1 = cbind(data1,as.vector(Group1[[call]]))
          }
          groupDataGroup1 = as.matrix(data1)
          
          Group = c(rep(1,nrow(Group1)))
          groupDataGroup1 = cbind(groupDataGroup1,Group)
          # print(groupDataGroup1)
          
          data2 = c()
          #print(Group1)
          for(call in alphabetH1){
            #print(as.vector(Group2[[call]]))
            data2 = cbind(data2,as.vector(Group2[[call]]))
          }
          groupDataGroup2 = as.matrix(data2)
          
          Group = c(rep(2,nrow(Group2)))
          groupDataGroup2 = cbind(groupDataGroup2,Group)
          colnames(groupDataGroup1) = c(alphabetH1, "Group")
          colnames(groupDataGroup2) = c(alphabetH1, "Group")
          #print(groupDataGroup2)
          
        }
        if (input$selectB == 2){
          Group1 = (Group1_Data$F2)
          
          colnames(Group1) = gsub("\\t", "\t", colnames(Group1))
          print(Group1)
          Group2 = (Group2_Data$F2)
          colnames(Group2) = gsub("\\t", "\t", colnames(Group2))
          #print(Group1[['C']])
          data1 = c()
          data2 = c()
          #print(Group1)
          for(call in colnames(Group1)){
            #print(as.vector(Group1[[call]]))
            data1 = cbind(data1,as.vector(Group1[[call]]))
            data2 = cbind(data2,as.vector(Group2[[call]]))
          }
          groupDataGroup1 = as.matrix(data1)
          
          Group = c(rep(1,nrow(Group1)))
          groupDataGroup1 = cbind(groupDataGroup1,Group)
          # print(groupDataGroup1)
          
          
          #print(Gro
          groupDataGroup2 = as.matrix(data2)
          
          Group = c(rep(2,nrow(Group2)))
          groupDataGroup2 = cbind(groupDataGroup2,Group)
          
          colnames(groupDataGroup1) = c(unlist(alphabets$H2_noTab), "Group")
          colnames(groupDataGroup2) = c(unlist(alphabets$H2_noTab), "Group")
          #print(groupDataGroup2)
        }
        
        if (input$selectB == 3){
          Group1 = (Group1_Data$F3)
          Group2 = (Group2_Data$F3)
          #print(Group1[['C']])
          data1 = c()
          data2 = c()
          #print(Group1)
          for(call in colnames(Group1)){
            #print(as.vector(Group1[[call]]))
            data1 = cbind(data1,as.vector(Group1[[call]]))
            data2 = cbind(data2,as.vector(Group2[[call]]))
          }
          groupDataGroup1 = as.matrix(data1)
          groupDataGroup2 = as.matrix(data2)
          Group = c(rep(1,nrow(Group1)))
          groupDataGroup1 = cbind(groupDataGroup1,Group)
          # print(groupDataGroup1)
          
          
          
          
          Group = c(rep(2,nrow(Group2)))
          groupDataGroup2 = cbind(groupDataGroup2,Group)
          
          
          colnames(groupDataGroup1) = c(unlist(alphabets$H3_noTab), "Group")
          colnames(groupDataGroup2) = c(unlist(alphabets$H3_noTab), "Group")
          #print(groupDataGroup2)
          
        }
        
        if (input$selectB == 4 & input$selectH == 4){
          Group1 = (Group1_Data$F4)
          Group2 = (Group2_Data$F4)
          #print(Group1[['C']])
          data1 = c()
          data2 = c()
          
          #print(Group1)
          for(call in colnames(Group1)){
            #print(as.vector(Group1[[call]]))
            data1 = cbind(data1,as.vector(Group1[[call]]))
            data2 = cbind(data2,as.vector(Group2[[call]]))
          }
          groupDataGroup1 = as.matrix(data1)
          
          Group = c(rep(1,nrow(Group1)))
          groupDataGroup1 = cbind(groupDataGroup1,Group)
          # print(groupDataGroup1)
          
          
          groupDataGroup2 = as.matrix(data2)
          
          Group = c(rep(2,nrow(Group2)))
          groupDataGroup2 = cbind(groupDataGroup2,Group)
          
          colnames(groupDataGroup1) = c(unlist(alphabets$H4_noTab), "Group")
          colnames(groupDataGroup2) = c(unlist(alphabets$H4_noTab), "Group")
          #print(groupDataGroup2)
          
        }else(return)
        
        
        set.seed(7777)
        
        #print(groupDataHT)
        
        borutaDF = rbind(groupDataGroup2,groupDataGroup1)
        #colnames(borutaDF)[ncol(borutaDF)] = "Group"
        # borutaDF[,"Group"] = as.factor( borutaDF[,"Group"])
        #print(typeof(borutaDF))
        borutaDF = as.data.frame(borutaDF)
        #colnames(borutaDF) = c(alphabetH1, "Group")
        
        print(borutaDF)
        
        b = Boruta(Group~.,data=borutaDF,pValue = 0.001)
        
        return(b)}
      else(return(NULL))
      
    })
    
    
    borutaplot = reactive({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        return(
          plot(calls.boruta, colCode = c("darkseagreen4", "goldenrod1", "firebrick", "dodgerblue3"),las = 2, cex.axis=.8))
        #plotImpHistory(b, xlab = "Classifier run",
        #ylab = "Importance")
      }
      else(stop("Upload folder") )
      
    })
    
    
    output$borutaplot = renderPlot({ 
      if(!is.null(boruta())){
        print(borutaplot())}
    })

    
    borutaStats = reactive({ 
      if(!is.null(boruta())){
        calls.boruta = boruta()
        stats = attStats(calls.boruta)
        statsConfirmed = subset(stats, decision == "Confirmed")
        return(statsConfirmed)
      }
      else(stop("Upload folder") )
      
    })

    output$bStats =  renderTable({
      borutaStats()
    },include.rownames=TRUE
    )
    output$borutaStats <- downloadHandler(
      filename = function(){
        paste("Boruta-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(borutaStats(), file, row.names = T)
      })
    

    
    output$download <- downloadHandler(
      filename = function() {
        paste("Harpia_Output", "zip", sep=".")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tempdir())
        
        pdf("HarpiaGraphics.pdf", width = 8, height = 6)
        print(plot1())
        if(input$label1 == ""){
          group = "Group1"
        }
        else{group = input$label1}
        Group1Data = EntropyAnalysisGroup1()
        `%notin%` <- Negate(`%in%`)
        if(input$label1 == ""){
          group = "Group1"
        }
        else{group = input$label1}
        markovdata = markovmodelR(Group1Data$counts2, input$pseudocount)
        print(markovdata$nodeSize)
        g.copy = 0
        g <- graph.adjacency(as.matrix(markovdata$TPM), weighted=TRUE)
        print(g)
        sortedWeights = sort(E(g)$weight, decreasing = T)[1:10]
        sortedWeights
        print(E(g)$weight)
        g.copy <- delete.edges(g, which(E(g)$weight %notin% sortedWeights))
        deg <- degree(g.copy, mode="all")
        V(g.copy)$size = rowMeans(Group1Data$counts1)*.6
        E(g.copy)$arrow.size <- 1.2
        E(g.copy)$edge.color <- "gray80"
        E(g.copy)$width <- E(g.copy)$weight*30
        V(g.copy)$label.cex = .7
        E(g.copy)$arrow.mode = 2
        V(g.copy)$color = "deepskyblue3"
        
        main = paste("Transition Graph for", group, sep = " ")
        print(plot(g.copy, main = main, layout=layout_in_circle(g.copy), vertex.label.color= "black",
                    vertex.label.family = "Helvetica", edge.label.font = 2))
        
        Group2Data = EntropyAnalysisGroup2()

        if(input$label2 == ""){
          group = "Group2"
        }
        else{group = input$label2}
        markovdata2 = markovmodelR(Group2Data$counts2, input$pseudocount)
        g.copy = 0
        g <- graph.adjacency(as.matrix(markovdata2$TPM), weighted=TRUE)
        print(g)
        sortedWeights = sort(E(g)$weight, decreasing = T)[1:10]
        sortedWeights
        print(E(g)$weight)
        g.copy <- delete.edges(g, which(E(g)$weight %notin% sortedWeights))
        deg <- degree(g.copy, mode="all")
        V(g.copy)$size = rowMeans(Group1Data$counts1)*.6
        E(g.copy)$arrow.size <- 1.2
        E(g.copy)$edge.color <- "gray80"
        E(g.copy)$width <- E(g.copy)$weight*30
        V(g.copy)$label.cex = .7
        E(g.copy)$arrow.mode = 2
        V(g.copy)$color = "firebrick3"
        
        main = paste("Transition Graph for", group, sep = " ")

        print(plot(g.copy, main = main, layout=layout_in_circle(g.copy), vertex.label.color= "black",
                   vertex.label.family = "Helvetica", edge.label.font = 2))

        MLEData = lmerAnalysis()
        mle = isolate(lmerAnalysis())
        print({qqnorm(resid(mle$mod1))
          qqline(resid(mle$mod1))})
        
        print(plot3())

        
        MLEData = MLEData$MLEData
        
        x = interaction(MLEData$Genotype,MLEData$Level)
        if(input$label1 != ""){
          label1 = input$label1
        }
        else{label1 = "Group1" }
        if(input$label2 != ""){
          label2 = input$label2
        }
        else{label2 = "Group2" }
        print(
          ggplot(MLEData, aes(x= x , y=Entropy,  fill= Genotype)) + geom_boxplot() + scale_fill_manual(labels=c(label1,label2),values=c('brown4','darkslategray')))
        #print(borutaplot())
        calls.boruta = boruta()
        plot(calls.boruta, colCode = c("darkseagreen4", "goldenrod1", "firebrick", "dodgerblue3"),las = 2, cex.axis=.4)
        #plotImpHistory(b, xlab = "Classifier run",ylab = "Importance")
        # print(borutaStats())
        dev.off()
        fs = c(fs, "HarpiaGraphics.pdf")
        stats  = attStats(calls.boruta)
        write.csv(stats, "BorutaStats.csv", row.names = T)
        #bstats = borutaStats()
        fs = c(fs, "BorutaStats.csv")
        
        out<-capture.output(summaryMLE())
        cat(out,file="LinearModel.txt",sep="\n",append=TRUE)
        
        fs = c(fs, "LinearModel.txt")
        
        zip(zipfile=fname, files=fs)
      },
      contentType = "application/zip"
    )
    
    
    ### PopOvers:
    
    addPopover(session=session, id="help1", title="", 
               content="Mixed-effects linear model where genotype is a fixed effect and entropy level is a random effect. Because is expected a different baseline entropy for each mouse and for the change in entropy between each entropy level to vary between mice, a random intercept, random slope model was applied.", placement = "bottom",
               trigger = "click", options = NULL)
    addPopover(session=session, id="help2", title="", 
               content="Markov Model graph for the transitions between two calls. Thickness of edges and size of nodes represent the relative proportion of a transition and call numbers, respectively. Complex calls are represented by red nodes and simple calls are represented by blue nodes.", placement = "bottom",
               trigger = "click", options = NULL)
    
    addPopover(session=session, id="help3", title="", 
               content="Sparse Partial Least Squares Determination Analysis is used to perform variable selection and classification in a one step procedure.", placement = "bottom",
               trigger = "click", options = NULL)
    
    addPopover(session=session, id="help4", title="", 
               content="Feature selection algorithm using Random Forest classification. It iteratively removes features proved to be less relevant than random probes", placement = "bottom",
               trigger = "click", options = NULL)
    
    
    observeEvent(input$refresh, {
      js$refresh();
    })

  })
  
})})

runApp(list(
  ui=ui,
  server=server
))