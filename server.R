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
  
  ###################################################
  ###################################################
  ####### COMPARE EFFECTS AND DISPLAY RESULTS #######
  ###################################################
  ###################################################
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  ligrecpairs = reactive({
    rl=read.csv("data/lig-rec.csv")
    org=input$org
    
    if(org=="human"){
      rl= rl %>% dplyr::select(Pair.Name:Receptor.ApprovedSymbol)
      rl$Pair.Name=toupper(rl$Pair.Name)
      rl$Ligand.ApprovedSymbol=toupper(rl$Ligand.ApprovedSymbol)
      rl$Receptor.ApprovedSymbol=toupper(rl$Receptor.ApprovedSymbol)
    }else if(org=="mouse"){
      rl= rl %>% dplyr::select(Mouse_LigandSym:Mouse.Pairs) %>% rename("Mouse.Pairs"="Pair.Name","Mouse_LigandSym"="Ligand.ApprovedSymbol","Mouse_RecSym"="Receptor.ApprovedSymbol")
    }
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
  
      rl=rl[(rl$Receptor.ApprovedSymbol %in% rgenes) & (rl$Ligand.ApprovedSymbol %in% lgenes),]
    # if(is.null(lgenes)==F & is.null(rgenes)==T ){
    #   rl=rl[rl$Ligand.ApprovedSymbol %in% lgenes,]
    # }else if(is.null(lgenes)==T & is.null(rgenes)==F){
    #   rl=rl[rl$Receptor.ApprovedSymbol %in% rgenes,]
    # }else if(is.null(lgenes)==F & is.null(rgenes)==F ){
    #   rl=rl[(rl$Receptor.ApprovedSymbol %in% rgenes) & (rl$Ligand.ApprovedSymbol %in% lgenes),]
    # }else{rl=rl}
    
    validate(
      need(nrow(rl)>=1, "No Ligand-Receptor Pairs found within the genelists uploaded")
    )
    recid=toupper(rl$Receptor.ApprovedSymbol)
    ligid=toupper(rl$Ligand.ApprovedSymbol)
    urlr= paste("http://www.genecards.org/cgi-bin/carddisp.pl?gene=",recid,sep = "")
    urll= paste("http://www.genecards.org/cgi-bin/carddisp.pl?gene=",ligid,sep = "")
    rl$Receptor.ApprovedSymbol=paste0("<a href='",urlr,"'target='_blank'>",rl$Receptor.ApprovedSymbol,"</a>")
    rl$Ligand.ApprovedSymbol=paste0("<a href='",urll,"'target='_blank'>",rl$Ligand.ApprovedSymbol,"</a>")
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
      DT::datatable(ligrecpairs(),
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
    filename = function() { paste(input$ligprj,"_",input$ligtype,"_",input$recprj,"_",input$rectype,'.csv', sep='') },
    content = function(file) {
      write.csv(ligrecpairs(), file,row.names=FALSE)
    })
  
  
}#end of server
