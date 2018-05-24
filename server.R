library(shiny)
library(shinyBS)
library(RColorBrewer)
library(ggplot2)
library(png)
library(d3heatmap)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(shinyRGL)
library(rgl)
library(rglwidget)
library(ggrepel)
library(readxl)
library(Biobase)
server <- function(input, output,session) {
  
  output$source <- renderUI({
    if(input$org=="Mouse"){rl=read.csv("data/Mm_PairsLigRec.csv")}else if(input$org=="Human"){rl=read.csv("data/Hs_PairsLigRec.csv")}
    options=as.character(unique(rl$Pair.Source))
    checkboxGroupInput('source', label='Select source(s)',choices=options,selected=options[1])
  })
  
  output$evidence <- renderUI({
    if(input$org=="Mouse"){rl=read.csv("data/Mm_PairsLigRec.csv")}else if(input$org=="Human"){rl=read.csv("data/Hs_PairsLigRec.csv")}
    options=as.character(unique(rl$Pair.Evidence))
    checkboxGroupInput('evidence',label='Select Evidence(s)',choices=options,selected=options[1])
  })
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  ligrecpairs = reactive({
    rl=read.csv("data/lig-rec.csv")
    org=input$org
    
    if(input$org=="Mouse"){rl=read.csv("data/Mm_PairsLigRec.csv")}else if(input$org=="Human"){rl=read.csv("data/Hs_PairsLigRec.csv")}
    validate(
      need(input$liggeneli, "Please Upload Ligand genelist")
    )
      lgene=input$liggeneli
      lgenes=read.table(lgene$datapath,stringsAsFactors = F)#get complete gene list as string
      lgenes=as.vector(lgenes$V1)
      lgenes=tolower(lgenes)
      lgenes=firstup(lgenes)
      
      validate(
        need(input$recgeneli, "Please Upload Receptor genelist")
      )
      rgene=input$recgeneli
      rgenes=read.table(rgene$datapath,stringsAsFactors = F)#get complete gene list as string
      rgenes=as.vector(rgenes$V1)
      rgenes=tolower(rgenes)
      rgenes=firstup(rgenes)
  
      rl=rl[(rl$receptor %in% rgenes) & (rl$ligand %in% lgenes),]
    # if(is.null(lgenes)==F & is.null(rgenes)==T ){
    #   rl=rl[rl$ligand %in% lgenes,]
    # }else if(is.null(lgenes)==T & is.null(rgenes)==F){
    #   rl=rl[rl$receptor %in% rgenes,]
    # }else if(is.null(lgenes)==F & is.null(rgenes)==F ){
    #   rl=rl[(rl$receptor %in% rgenes) & (rl$ligand %in% lgenes),]
    # }else{rl=rl}
      if(input$checksource==T){rl=rl[rl$Pair.Source==input$source,]}
      if(input$checkevi==T){rl=rl[rl$Pair.Evidence==input$evidence,]}
    
    return(rl)
  })
  
  table = reactive({
    rl=ligrecpairs()
    validate(
      need(nrow(rl)>=1, "No Ligand-Receptor Pairs found within the genelists uploaded")
    )
    recid=toupper(rl$receptor)
    ligid=toupper(rl$ligand)
    urlr= paste("http://www.genecards.org/cgi-bin/carddisp.pl?gene=",recid,sep = "")
    urll= paste("http://www.genecards.org/cgi-bin/carddisp.pl?gene=",ligid,sep = "")
    rl$receptor=paste0("<a href='",urlr,"'target='_blank'>",rl$receptor,"</a>")
    rl$ligand=paste0("<a href='",urll,"'target='_blank'>",rl$ligand,"</a>")
    # if(input$source!="All"){rl=rl[rl$Pair.Source==input$source,]}
    # if(input$evidence!="All"){rl=rl[rl$Pair.Evidence==input$evidence,]}
    return(rl)
  })
  
  #print data TABLE
  output$ligrecpairs = DT::renderDataTable({
    input$receptor
    input$ligand
    input$recprj
    input$ligprj
    input$rectype
    input$ligtype
    input$liggene
    input$recgene
    input$liggeneli
    input$recgeneli
    withProgress(session = session, message = 'Generating...',detail = 'Please Wait...',{
      DT::datatable(table(),
                    extensions = c('Buttons','Scroller'),
                    options = list(dom = 'Bfrtip',
                                   searchHighlight = TRUE,
                                   pageLength = 10,
                                   lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                   scrollX = TRUE,
                                   buttons = c('copy', 'print')
                    ),rownames=FALSE,escape = F,selection = list(mode = 'single', selected =1))
    })
  })
  
  output$dwldtab = renderUI({
    downloadButton('dwldtable','Download')
  }) 
  
  output$dwldtable <- downloadHandler(
    filename = function() { "ligand_receptor_pairs.csv" },
    content = function(file) {
      write.csv(ligrecpairs(), file,row.names=FALSE)
    })
  
  
}#end of server
