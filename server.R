shinyServer(function(session, input, output){
  options(shiny.maxRequestSize = 5*1024^3) #setting maximum filesize to 5gb
  # observeEvent(input$primeData,{
  #   # infile0 <- input$primeData
  #   # if(is.null(infile0)){
  #   #   return(NULL)
  #   # }else{
  #   #   primeItem <- read.csv(infile0$datapath)
  #   # }
  #   wireframeNodeClusterSalesData <- dataPrep()[[5]]
  #   ClusterColumn <- colnames(primeItem)[grepl('cluster', tolower(colnames(primeItem)))][1]
  #   allCluster <- unique(primeItem[, ClusterColumn]) %>% as.character()
  #   whichNat <- which(grepl('national', unique(primeItem[, ClusterColumn]), ignore.case = TRUE))
  #   print(allCluster)
  #   print(whichNat)
  #   ClusterChoice <- c(allCluster[whichNat], allCluster[-whichNat])
  #   print(ClusterChoice)
  #   updateSelectInput(session, 'cluster', choices = ClusterChoice %>% toupper())
  #   updateSelectInput(session, 'strCnt', choices = colnames(wireframeNodeClusterSalesData)[grepl('str_cnt', tolower(colnames(wireframeNodeClusterSalesData)))], selected = 'CUR_VALID_STR_CNT')
  # })
  
  selectedNode <- reactiveValues(nodeNbr=NULL, nodeName=NULL, nodeLevel=NULL)

  dataPrep <- reactive({
    infile0 <- input$primeData
    if(is.null(infile0)){
      return(NULL)
    }else{
      primeItem <- read.csv(infile0$datapath)
    }


    if(!is.null(infile0)){
      print('data prep')
      levelColName <- colnames(primeItem)[grepl('level', tolower(colnames(primeItem)))]
      wireframeNodeCols <- levelColName[grepl('node', tolower(levelColName))]
      wireframeInput <- primeItem[,wireframeNodeCols] %>% t()
      wireframeInput <- wireframeInput[complete.cases(wireframeInput),] %>% t()
      wireframeInput <- wireframeInput[which(wireframeInput[,1] != ""),]
      wireframeSalesCompare <- primeItem[,c(colnames(primeItem)[grepl('rollup', tolower(colnames(primeItem)))][1:2], wireframeInput %>% colnames(), 'TY.L52.Sales', 'LY.L52.Sales')] %>% unique()
      wireframeSalesCompare <- wireframeSalesCompare[which(wireframeSalesCompare[,3] != ""),]
      
      # wireframeInput <- wireframeNodeData[,grepl('level', colnames(wireframeNodeData), ignore.case = TRUE)]
      # rootNode <- data.frame('Level.0.Node' = rep('All', nrow(wireframeInput)))
      # wireframeInput <- cbind(rootNode, wireframeInput)
      for(i in 1:ncol(wireframeInput)){
        wireframeInput[,i] <- paste(colnames(wireframeInput)[i], toupper(wireframeInput[,i]), sep = '---')
        wireframeSalesCompare[,i+2] <- paste(colnames(wireframeSalesCompare)[i+2], toupper(wireframeSalesCompare[,i+2]), sep = '---')
      }
      cdt_level=wireframeInput %>% unique()
      # wireframeClusterSalesData <- inner_join(wireframeClusterData, wireframeSalesData)
      # wireframeNodeClusterSalesData <- inner_join(cbind(wireframeNodeData[,1:2], wireframeInput), wireframeClusterSalesData, by=c('info.Rollup.ID'='ROLLUP_ID'))

      storeCount <- colnames(primeItem)[grepl('str_cnt', tolower(colnames(primeItem)))]
      rollupCluster <- c(colnames(primeItem)[grepl('rollup', tolower(colnames(primeItem)))][1:2], colnames(primeItem)[grepl('cluster', tolower(colnames(primeItem)))][1])
      pspw <- colnames(primeItem)[grepl('psw', tolower(colnames(primeItem)))]
      pspw <- pspw[!grepl('sub', pspw)]
      wireframeNodeClusterSalesData <- primeItem[,c(rollupCluster, colnames(wireframeInput), storeCount, pspw)]
      wireframeNodeClusterSalesData <- wireframeNodeClusterSalesData[which(wireframeNodeClusterSalesData[,4] != ""),]

      j <- c()
      for(i in 1:ncol(wireframeNodeClusterSalesData)){
        if(is.character(wireframeNodeClusterSalesData[,i]))
          wireframeNodeClusterSalesData[,i] <- toupper(wireframeNodeClusterSalesData[,i])
        if(colnames(wireframeNodeClusterSalesData)[i] %in% colnames(wireframeInput))
          wireframeNodeClusterSalesData[,i] <- paste(colnames(wireframeNodeClusterSalesData)[i], toupper(wireframeNodeClusterSalesData[,i]), sep = '---')
        if(is.factor(wireframeNodeClusterSalesData[,i])){
          wireframeNodeClusterSalesData[,i] <- as.character(wireframeNodeClusterSalesData[,i])
          wireframeNodeClusterSalesData[,i] <- toupper(wireframeNodeClusterSalesData[,i])
        }
        if(sum(is.na(wireframeNodeClusterSalesData[,i])) == nrow(wireframeNodeClusterSalesData))
          j <- c(j, i)
      }

      wireframeNodeClusterSalesData <- wireframeNodeClusterSalesData[,-j]
      wireframeNodeClusterSalesData <- unique(wireframeNodeClusterSalesData)

      cdt_level_array = as.data.frame(sapply(sapply(cdt_level, as.character), as.character))
      cdt_matrix1 = data.matrix(cdt_level_array)
      cdt_matrix = matrix(cdt_matrix1,byrow = F,ncol = ncol(cdt_level))
      cdt_matrix = as.matrix(data.frame(cdt_matrix) %>% arrange(X1))

      node_label = cbind(cdt_matrix1,cdt_level_array);colnames(node_label)=c("number","node")
      node_label = distinct(node_label)
      #print(node_label)
      #str(node_label)
      node_label$node <- as.character(node_label$node)
      level=unique(cdt_matrix[,1])

      number=c();col_number = c();row_num = c();c=1 ;  p1=0
      for(l1 in level){
        i1=0
        count = length(which(cdt_matrix[,1]%in%l1))
        if(count == 1){
          temp = cdt_matrix[which(cdt_matrix[,1]%in%l1),]
          temp=matrix(temp,nrow = 1)
        }else{
          temp = cdt_matrix[which(cdt_matrix[,1]%in%l1),]
        }

        for(i in 1:ncol(temp)){
          for(j in 1:nrow(temp)){
            if(!(temp[j,i] %in% number)){
              p=which(level == temp[j,1])
              if(p != p1){c=1};p1=p
              row_num = c(row_num,p)
              number = c(number,temp[j,i])
              if(i!=i1){
                col_number = c(col_number,c);d=c; c=c+1 ;
              }else{
                col_number = c(col_number,d)
              }
              i1=i
            }
          }
        }
      }
      col_matrix = as.data.frame(cbind(cbind(number,col_number),row_num))
      cdt_matrix_unique <- cdt_matrix %>% unique() %>% as.data.frame() %>% arrange_(., .dots = lapply(colnames(.), as.symbol)) %>% as.matrix()
      node_label <- data.frame(lapply(node_label, as.character), stringsAsFactors=FALSE)
      
      if(input$strCnt == ''){
        strCnt <- 'CUR_VALID_STR_CNT'
      }else{
        strCnt <- input$strCnt
      }
      
      allStrCnt <- colnames(wireframeNodeClusterSalesData)[grepl('str_cnt', tolower(colnames(wireframeNodeClusterSalesData)))]
      
      POD_data <- vector(mode = 'list', sum(grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)))
      #colnames(wireframeNodeClusterSalesData) %>% print()
      #c(rollupCluster, colnames(wireframeInput), input$strCnt, pspw) %>% print()
      wireframeNodeClusterSalesData <- wireframeNodeClusterSalesData[,c(rollupCluster, colnames(wireframeInput), strCnt, pspw)]
      wireframeNodeClusterSalesData <- inner_join(wireframeSalesCompare, wireframeNodeClusterSalesData)
      
      colnames(wireframeNodeClusterSalesData)[grepl(strCnt, colnames(wireframeNodeClusterSalesData))] <- 'Store_Count'
      whichCluster <- grepl('cluster', tolower(colnames(wireframeNodeClusterSalesData)))
      whichPSW <- grepl('psw', tolower(colnames(wireframeNodeClusterSalesData)))
      colnames(wireframeNodeClusterSalesData)[whichCluster] <- 'CLUSTER'
      colnames(wireframeNodeClusterSalesData)[whichPSW] <- c('UPSPW', 'X.PSPW')
      wireframeNodeClusterSalesGrowthData <- wireframeNodeClusterSalesData %>% 
        mutate(., diff = TY.L52.Sales - LY.L52.Sales) %>% 
        mutate(., growth = (100 * diff / LY.L52.Sales %>% abs()) %>% round(., 3)) %>% 
        mutate(., dummy = growth) %>% 
        as.data.frame() %>% unique()
      wireframeNodeClusterSalesGrowthData$dummy[which(is.nan(wireframeNodeClusterSalesGrowthData$growth))] <- NA
      wireframeNodeClusterSalesGrowthData$dummy[which(is.infinite(wireframeNodeClusterSalesGrowthData$growth))] <- NA
      
      
      SalesCompare <- vector(mode = 'list', sum(grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)))
      for(i in 1:(SalesCompare %>% length())){
        levelNames <- colnames(wireframeNodeClusterSalesData)[grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)]
        SalesCompare[[i]] <- wireframeNodeClusterSalesGrowthData %>% group_by_(levelNames[i]) %>% 
          summarise(growth = mean(dummy, na.rm = TRUE) %>% round(., 2))
        colnames(SalesCompare[[i]])[1] <- 'NODE'
      }
      SalesCompare <- do.call('rbind', SalesCompare) %>% data.frame()
      
      for (i in 1:(POD_data %>% length())) {
        levelNames <- colnames(wireframeNodeClusterSalesData)[grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)]
        POD_data[[i]] <- wireframeNodeClusterSalesData %>% group_by_(levelNames[i], 'CLUSTER') %>%
          summarise('POD' = 0.5*52*(sum(UPSPW, na.rm = TRUE) + sum(X.PSPW, na.rm = TRUE))/sum(Store_Count, na.rm=TRUE)) %>%
          mutate(., level = i) %>% as.data.frame()
        colnames(POD_data[[i]])[1] <- 'NODE'
        temp_list <- split(POD_data[[i]], POD_data[[i]]$CLUSTER)
        temp_list <- lapply(temp_list, function(x) x %>% arrange(., desc(POD)) %>% mutate(rank = 1:nrow(.)) %>% as.data.frame)
        POD_data[[i]] <- do.call('rbind', temp_list)
      }
      POD_data_Cluster <- do.call('rbind', POD_data)
      POD_data_Cluster <- inner_join(POD_data_Cluster, SalesCompare)
      
      LoyaltyCols <- levelColName[grepl('item.loyalty', tolower(levelColName))][1:ncol(wireframeInput)]
      Loyalty <- primeItem[,c(rollupCluster, LoyaltyCols, colnames(wireframeInput))] %>% unique()
      colnames(Loyalty)[grepl('cluster', tolower(colnames(Loyalty)))] <- 'CLUSTER'
      for(i in 1:ncol(Loyalty)){
        Loyalty[,i] <- toupper(Loyalty[,i])
        if(colnames(Loyalty)[i] %in% colnames(wireframeInput))
          Loyalty[,i] <- paste(colnames(Loyalty)[i], Loyalty[,i], sep = '---')
      }
      return(list(POD_data_Cluster, cdt_matrix_unique, col_matrix, node_label, wireframeNodeClusterSalesData, allStrCnt, wireframeNodeClusterSalesGrowthData, Loyalty))
    }
  })

  observeEvent(input$primeData,{
    # infile0 <- input$primeData
    # if(is.null(infile0)){
    #   return(NULL)
    # }else{
    #   primeItem <- read.csv(infile0$datapath)
    # }
    wireframeNodeClusterSalesData <- dataPrep()[[5]]
    
    allCluster <- unique(wireframeNodeClusterSalesData[,'CLUSTER']) %>% as.character()
    
    whichNat <- which(grepl('national', allCluster, ignore.case = TRUE))
    print(allCluster)
    print(whichNat)
    ClusterChoice <- c(allCluster[whichNat], allCluster[-whichNat])
    print(ClusterChoice)
    updateSelectInput(session, 'cluster', choices = ClusterChoice %>% toupper())
    updateSelectInput(session, 'strCnt', choices = dataPrep()[[6]], selected = 'CUR_VALID_STR_CNT')
  })
  
  observe(if(is.null(dataPrep())){
    hide(id = 'afterDataLoad')
  }else{
    show(id = 'afterDataLoad')
  })

  output$wireframe <- renderPlot({
    POD_data_Cluster <<- dataPrep()[[1]]
    cdt_matrix_unique <<- dataPrep()[[2]]
    col_matrix <<- dataPrep()[[3]]
    node_label <<- dataPrep()[[4]]

    if(is.null(dataPrep())){
      #return('Please Input the required Data to View the Wireframe HeatMap')
      plot(1,type="n",axes = F, xlab = '', ylab = '')
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 'white')
      text(1,1.1, 'Please Input the required Data to View the Wireframe HeatMap', cex = 3, font = 2)
    }else{
      POD_data_Cluster_Selected <- POD_data_Cluster[which(POD_data_Cluster$CLUSTER == input$cluster),]
      layout(cdt_matrix_unique) #%>% layout.show()
      par(mar=rep(0.05,4))
      #require(RColorBrewer)||installed.packages(RColorBrewer)
      ind = 1:max(cdt_matrix_unique)
      for(i in ind)
      {
        whichLevel = col_matrix$col_number[which(col_matrix$number == i)]
        whichNode <- as.character(node_label$node[which(node_label$number == i)])
        if(input$shades == 'Viridis')
          #allColor <- colorRampPalette(c('midnightblue', 'royalblue', 'steelblue3', 'cyan', 'lightblue1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
          allColor <- viridis(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        if(input$shades == 'Magma')
          #allColor <- colorRampPalette(c('dark red', 'red', 'orangered', 'orange'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
          allColor <- magma(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        if(input$shades == 'Inferno')
          #allColor <- colorRampPalette(c('dark green', 'green', 'light green', 'darkseagreen1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
          allColor <- inferno(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        if(input$shades == 'Plasma')
          #allColor <- colorRampPalette(c('dark green', 'green', 'light green', 'darkseagreen1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
          allColor <- plasma(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        rank <- POD_data_Cluster_Selected$rank[which(POD_data_Cluster_Selected$level == whichLevel & POD_data_Cluster_Selected$NODE == whichNode)]
        color <- allColor[rank]
        
        plot(1,type="n",axes = F)
        if(is.null(selectedNode$nodeNbr)){
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color)
          if(rank == 1){
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "red",lwd = 8)
            text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, col = 'white', font = 2)
          }else{
            text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, font = 2)
          }
        }else{
          if(i %in% selectedNode$nodeNbr){
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 'dark orange')
            if(rank == 1){
              rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "red",lwd = 8)
              text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, col = 'white', font = 2)
            }else{
              rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "black",lwd = 8)
              text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, font = 2)
            }
          }else{
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color)
            if(rank == 1){
              rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "red",lwd = 8)
              text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, col = 'white', font = 2)
            }else{
              text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, font = 2)
            }
          }
        }
        growthRow <- POD_data_Cluster_Selected[which(POD_data_Cluster_Selected$NODE == whichNode), c('NODE', 'growth')]
        if(!is.nan(growthRow[,2]) & sign(growthRow[,2]) == 1){
          mtext(paste(paste0(abs(growthRow[,2]), '%'), intToUtf8(9650)), side = 1, line = -1.3, col = 'green3', cex = 1.3)
        }else{
          if(!is.nan(growthRow[,2])){
            mtext(paste(paste0(abs(growthRow[,2]), '%'), intToUtf8(9660)), side = 1, line = -1.3, col = 'red', cex = 1.3)
          }else{
            mtext("Couldn't Infer", side = 1, line = -2, col = 'white', cex = 1.3)
          }
        }
      }
      # #par(par_default)
      # par(mai=rep(0.3,4))
      # node_level = paste("Level",1:ncol(cdt_matrix_unique)-1)
      # #text((1:length(node_level)-1)/(length(node_level)-0.4),1.2,node_level,pos = 3)
      # mtext(node_level, at = (1:length(node_level)-1)/(length(node_level) - 0.8))
    }
  })

  output$colorScale <- renderPlot({
      showCol <- function (colours, labels = FALSE, borders = NA) {
        n <- length(colours)
        ncol <- n
        nrow <- 1
        colours <- c(rev(colours), rep(NA, nrow * ncol - length(colours)))
        colours <- matrix(colours, ncol = ncol, byrow = TRUE)
        old <- par(pty = "m", mar = c(0, 0, 0, 0))
        on.exit(par(old))
        size <- max(dim(colours))
        plot(c(0.6, size-0.6), c(-0.2, -0.8), type = "n", xlab = "", ylab = "",
             axes = FALSE)
        rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
          #par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = colours, border = borders)
        if (labels) {
          text(col(colours) - 0.5, -row(colours) + 0.5, colours)
        }
      }

      cdt_matrix_unique <- dataPrep()[[2]]
      n <- cdt_matrix_unique[,ncol(cdt_matrix_unique)] %>% unique() %>% length()
      if(input$shades == 'Viridis')
        #allColor <- colorRampPalette(c('midnightblue', 'royalblue', 'steelblue3', 'cyan', 'lightblue1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        return(showCol(viridis(n)))
      if(input$shades == 'Magma')
        #allColor <- colorRampPalette(c('dark red', 'red', 'orangered', 'orange'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        return(showCol(magma(n)))
      if(input$shades == 'Inferno')
        #allColor <- colorRampPalette(c('dark green', 'green', 'light green', 'darkseagreen1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        return(showCol(inferno(n)))
      if(input$shades == 'Plasma')
        #allColor <- colorRampPalette(c('dark green', 'green', 'light green', 'darkseagreen1'))(length(which(POD_data_Cluster_Selected$level == whichLevel)))
        return(showCol(plasma(n)))
    })

  observeEvent(input$plotClick,{
    click <- input$plotClick
    cdt_matrix_unique <- dataPrep()[[2]]
    col_matrix <- dataPrep()[[3]]
    node_label <- dataPrep()[[4]]
    
    left <- click$domain$left
    right <- click$domain$right
    top <- click$domain$top
    bottom <- click$domain$bottom
    n <- ncol(cdt_matrix_unique)

    y <- click$y
    x <- click$x

    eachWidth <- (right-left)/n
    getLevel <- sapply(1:n, function(z) ifelse(x < (left + eachWidth*z) && x >= (left + eachWidth*(z-1)), TRUE, FALSE))
    getLevel <- which(getLevel == TRUE)

    n1 <- cdt_matrix_unique[,getLevel] %>% length()
    eachHeight <- (top-bottom)/n1
    nodeDF <- cdt_matrix_unique[,getLevel] %>% table() %>% as.data.frame()
    nodeDF$. <- as.numeric(as.character(nodeDF$.))
    nodeDF <- inner_join(cdt_matrix_unique[,getLevel] %>% as.data.frame(), nodeDF) %>% unique()
    colnames(nodeDF) <- c('NodeNbr', 'Freq')
    nodeDF$startCumSum <- c(0, cumsum(nodeDF$Freq)[-length(unique(nodeDF$NodeNbr))])
    nodeDF$endCumSum <- cumsum(nodeDF$Freq)
    nodeDF$Start <- top - eachHeight*nodeDF$startCumSum
    nodeDF$End <- top - eachHeight*nodeDF$endCumSum

    nodeNbr <- nodeDF$NodeNbr[which(nodeDF$Start > y & nodeDF$End <= y)]
    nodeName <- node_label$node[which(node_label$number == nodeNbr)] %>% strsplit(., split = '---', fixed = TRUE) %>% unlist() %>% tail(1)
    nodeLevel <- node_label$node[which(node_label$number == nodeNbr)] %>% strsplit(., split = '---', fixed = TRUE) %>% unlist() %>% head(1)
    

    if(is.null(selectedNode$nodeNbr)){
      selectedNode$nodeNbr <- nodeNbr
      selectedNode$nodeName <- nodeName
      selectedNode$nodeLevel <- nodeLevel
    }else{
      if(selectedNode$nodeLevel == nodeLevel){
        if(nodeNbr %in% selectedNode$nodeNbr){
          del <- which(selectedNode$nodeNbr == nodeNbr)
          if(length(selectedNode$nodeNbr) == 1){
            selectedNode$nodeNbr <- NULL
            selectedNode$nodeName <- NULL
            selectedNode$nodeLevel <- NULL
          }else{
            selectedNode$nodeNbr <- selectedNode$nodeNbr[-del]
            selectedNode$nodeName <- selectedNode$nodeName[-del]
            selectedNode$nodeLevel <- selectedNode$nodeLevel[-del]
          }
        }else{
          selectedNode$nodeNbr <- c(selectedNode$nodeNbr, nodeNbr)
          selectedNode$nodeName <- c(selectedNode$nodeName, nodeName)
          selectedNode$nodeLevel <- c(selectedNode$nodeLevel, nodeLevel)
        }
      }else{
        updateSliderInput(session, inputId = 'storeSlider', value = 0)
        selectedNode$nodeNbr <- nodeNbr
        selectedNode$nodeName <- nodeName
        selectedNode$nodeLevel <- nodeLevel
      }
    }
  })
  
  observeEvent(input$strCnt,{
    updateSliderInput(session, inputId = 'storeSlider', value = 0)
  })
  
  getDF <- reactive({
    wireframeNodeClusterSalesData <<- dataPrep()[[7]]
    print(colnames(wireframeNodeClusterSalesData))
    selectedNode$nodeNbr -> nodeNbr
    selectedNode$nodeName -> nodeName
    selectedNode$nodeLevel -> nodeLevel
    Loyalty <- dataPrep()[[8]]
    nodeLevel <- unique(nodeLevel)
    
    loyaltyLevel <- gsub('Node', 'Item.Loyalty', nodeLevel)
    loyaltySmall <- Loyalty[which(Loyalty$CLUSTER == input$cluster), c(1, 2, which(colnames(Loyalty) %in% c(nodeLevel, loyaltyLevel)))]
    loyaltySmall <- loyaltySmall[which(loyaltySmall[,nodeLevel] %in% paste(nodeLevel, nodeName, sep = '---')),]
    
    rollupCluster <- colnames(wireframeNodeClusterSalesData)[grepl('rollup', tolower(colnames(wireframeNodeClusterSalesData)))]
    smallData <- wireframeNodeClusterSalesData[,c(nodeLevel, 'CLUSTER', rollupCluster, 'TY.L52.Sales', 'LY.L52.Sales', 'growth', 'Store_Count', 'UPSPW', 'X.PSPW')]
    
    selectedNodes <- paste(nodeLevel, nodeName, sep = '---')
    returnDFNode <- smallData[which(smallData[,1] %in% selectedNodes),]
    returnDF <- returnDFNode[which(returnDFNode[,2] == input$cluster),]
    updateSliderInput(session, inputId = 'storeSlider', max = max(returnDF$Store_Count, na.rm = TRUE))
    
    whichNA <- which(is.na(returnDF$Store_Count %>% as.numeric()))
    #print(whichNA)
    if(input$storeSlider == 0){
      returnDF$PerformancePerWeek <- rowMeans(returnDF[,c('UPSPW', 'X.PSPW')], na.rm = TRUE)
      DF <- returnDF
      q3 <- quantile(DF$PerformancePerWeek, probs = 0.75, na.rm = TRUE)
      q1 <- quantile(DF$Store_Count, probs = 0.25, na.rm = TRUE)
      whichInc <- which(DF$Store_Count < q1 & DF$PerformancePerWeek > q3)
    }else{
      if(length(whichNA) == 0){
        finalDF <- returnDF[which(returnDF$Store_Count >= input$storeSlider),]
      }else{
        finalDF <- returnDF[-c(whichNA, which(returnDF$Store_Count %>% as.numeric() < input$storeSlider %>% as.numeric())),]
      }
      finalDF$PerformancePerWeek <- rowMeans(finalDF[,c('UPSPW', 'X.PSPW')], na.rm = TRUE)
      # q3 <- quantile(finalDF$PerformancePerWeek, probs = 0.75, na.rm = TRUE)
      # q1 <- quantile(finalDF[,4], probs = 0.25, na.rm = TRUE)
      # whichInc <<- which(finalDF[,4] < q1 & finalDF$PerformancePerWeek > q3)
      DF <- finalDF
      q3 <- quantile(DF$PerformancePerWeek, probs = 0.75, na.rm = TRUE)
      q1 <- quantile(DF$Store_Count, probs = 0.25, na.rm = TRUE)
      whichInc <- which(DF$Store_Count < q1 & DF$PerformancePerWeek > q3)
    }
    whichInf <- which(is.infinite(DF$growth))
    DF$growth[whichInf] <- rep('New Item', length(whichInf))
    loyaltySmall[,1] <- as.numeric(loyaltySmall[,1])
    DF <- inner_join(DF, loyaltySmall)
    return(list(as.data.frame(DF), as.numeric(whichInc)))
  })

  observe({
    addTooltip(session, id = 'save', title = 'Your selection is Successfully saved', trigger = 'click', placement = 'top')
    if(is.null(selectedNode$nodeNbr)){
      hide(id = 'table')
    }else{
      show(id = 'table')
      DF <- getDF()[[1]] %>% as.data.frame()
      whichInc <- getDF()[[2]] %>% as.numeric()
      #print(whichInc)
      output$itemProps <- renderDataTable(DF %>% as.data.frame(), rownames = FALSE, selection = list(mode = 'multiple', target = 'row', selected = whichInc),
                                          colnames = c(colnames(getDF()[[1]])[1], 'Cluster', 'RollupID', 'RollupDescription', 'ThisYear DollarSales', 'LastYear DollarSales', 'Growth', 'Selected StoreCount', 'Units PerStore PerWeek', 'DollarSales PerStore PerWeek', 'Performance PerStore PerWeek', 'Loyalty'),
                                          options = list(pageLength = 20, scrollX = TRUE, sDom  = '<"top">t<"bottom">ip', columnDefs = list(list(className = 'dt-center', targets = 0:11))))
    }
  })
  
  expand <- reactiveValues(df = data.frame())
  observeEvent(input$save,{
    delay(500, {
      removeTooltip(session, 'save')
      })
    
    df <- getDF()[[1]][input$itemProps_rows_selected,]
    df$StoreCountType <- input$strCnt
    colnames(df)[1] <- 'Node Level'
    expand$df <- rbind(expand$df, df)
  })
  output$download <- downloadHandler(filename = 'ItemsSelectedForExpansion.csv',
                                     content = function(file)
                                       write.csv(expand$df, file, row.names = FALSE)
                                     )
  
  
})