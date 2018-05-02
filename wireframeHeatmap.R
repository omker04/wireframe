library(dplyr)
setwd('/Users/omahala/Desktop/Demand Transference/Wireframe Generation/')

primeItem <- read.csv('prime_item_data_boxDinners.csv')
# levelColName <- colnames(primeItem)[grepl('level', tolower(colnames(primeItem)))]
# wireframeNodeCols <- levelColName[grepl('node', tolower(levelColName))]
# wireframeInput <- primeItem[,wireframeNodeCols] %>% t()
# wireframeInput <- wireframeInput[complete.cases(wireframeInput),] %>% t()
# wireframeInput <- wireframeInput[which(wireframeInput[,1] != ""),]

# wireframeNodeData <- read.csv('node_data.csv')
# wireframeClusterData <- read.csv('cluster_data.csv')
# wireframeSalesData <- read.csv('cluster_wise_units_and_sales.csv')


#wireframeNodeData <- wireframeNodeData[which(wireframeNodeData$Level.1.Node != ""),]

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
wireframeSalesCompare$Rollup.Description <- toupper(wireframeSalesCompare$Rollup.Description)

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
# whichCluster <- grepl('cluster', tolower(colnames(wireframeNodeClusterSalesData)))
# whichPSW <- grepl('psw', tolower(colnames(wireframeNodeClusterSalesData)))
# wireframeClusterSalesData <- inner_join(wireframeClusterData, wireframeSalesData)
# wireframeNodeClusterSalesData <- inner_join(cbind(wireframeNodeData[,1:2], wireframeInput), wireframeClusterSalesData, by=c('info.Rollup.ID'='ROLLUP_ID'))
wireframeNodeClusterSalesData <- unique(wireframeNodeClusterSalesData)


#wireframeNodeClusterData
#wireframeNodeClusterData <- inner_join(wireframeNodeClusterData, wireframeSalesData, by = c('info.Rollup.ID'='ROLLUP_ID'))

cdt_level_array = as.data.frame(sapply(sapply(cdt_level, as.character), as.character))
cdt_matrix1 = data.matrix(cdt_level_array)
cdt_matrix = matrix(cdt_matrix1,byrow = F,ncol = ncol(cdt_level))
cdt_matrix = as.matrix(data.frame(cdt_matrix) %>% arrange(X1))


node_label = cbind(cdt_matrix1,cdt_level_array);colnames(node_label)=c("number","node")
node_label = distinct(node_label)
print(node_label)
str(node_label)
node_label$node <- as.character(node_label$node)
#final_level = merge(final_level,node_label)

level=unique(cdt_matrix[,1])

number=c();col_number = c();row_num = c();c=1 ;  p1=0
for(l1 in level)
{
  i1=0
  count = length(which(cdt_matrix[,1]%in%l1))
  if(count == 1)
  {
    temp = cdt_matrix[which(cdt_matrix[,1]%in%l1),]
    temp=matrix(temp,nrow = 1)
  } else
  {
    temp = cdt_matrix[which(cdt_matrix[,1]%in%l1),]
  }
  
  for(i in 1:ncol(temp))
  {
    for(j in 1:nrow(temp))
    {
      if(!(temp[j,i] %in% number))
      {
        p=which(level == temp[j,1])
        if(p != p1){c=1};p1=p
        row_num = c(row_num,p)
        number = c(number,temp[j,i])
        if(i!=i1)
        {
          col_number = c(col_number,c);d=c; c=c+1 ;
        }else
        {
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
#node_label$node <- lapply(strsplit(node_label$node, split = '---', fixed = TRUE), function(x) x[2]) %>% unlist()


POD_data <- vector(mode = 'list', sum(grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)))
wireframeNodeClusterSalesData <- wireframeNodeClusterSalesData[,c(rollupCluster, colnames(wireframeInput), 'CUR_VALID_STR_CNT', pspw)]
colnames(wireframeNodeClusterSalesData)[grepl('CUR_VALID_STR_CNT', colnames(wireframeNodeClusterSalesData))] <- 'Store_Count'
whichCluster <- grepl('cluster', tolower(colnames(wireframeNodeClusterSalesData)))
whichPSW <- grepl('psw', tolower(colnames(wireframeNodeClusterSalesData)))
colnames(wireframeNodeClusterSalesData)[whichCluster] <- 'CLUSTER'
colnames(wireframeNodeClusterSalesData)[whichPSW] <- c('UPSPW', 'X.PSPW')

for (i in 1:(POD_data %>% length())) {
  levelNames <- colnames(wireframeNodeClusterSalesData)[grepl('level', colnames(wireframeNodeClusterSalesData), ignore.case = TRUE)]
  POD_data[[i]] <- wireframeNodeClusterSalesData %>% group_by_(levelNames[i], 'CLUSTER') %>% 
    summarise("POD" = 0.5*52*(sum(UPSPW, na.rm = TRUE) + sum(X.PSPW, na.rm = TRUE))/sum(Store_Count, na.rm=TRUE)) %>% 
    mutate(., level = i) %>% as.data.frame()
  colnames(POD_data[[i]])[1] <- 'NODE'
  temp_list <- split(POD_data[[i]], POD_data[[i]]$CLUSTER)
  temp_list <- lapply(temp_list, function(x) x %>% arrange(., desc(POD)) %>% mutate(rank = 1:nrow(.)) %>% as.data.frame)
  POD_data[[i]] <- do.call('rbind', temp_list)
}

POD_data_Cluster <- do.call('rbind', POD_data)

{
# POD_data_Cluster <- POD_data_Cluster[which(POD_data_Cluster$CLUSTER == selectedCluster),]
# 
layout(cdt_matrix_unique) #%>% layout.show()
par(mar=rep(0.1,4))
require(RColorBrewer)||installed.packages(RColorBrewer)
ind = 1:max(cdt_matrix_unique)
for(i in ind)
{
  whichLevel = col_matrix$col_number[which(col_matrix$number == i)]
  #row_num1=col_matrix$row_num[which(col_matrix$number == i)]-1
  whichNode <- as.character(node_label$node[which(node_label$number == i)])
  allColor <- colorRampPalette(c('midnightblue', 'royalblue', 'steelblue3', 'cyan', 'lightblue1'))(length(which(POD_data_Cluster$level == whichLevel)))
  rank <- POD_data_Cluster$rank[which(POD_data_Cluster$level == whichLevel & POD_data_Cluster$NODE == whichNode)]
  color <- allColor[rank]

  plot(1,type="n",axes = F)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = color)

  if(rank == 1)
  {
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "yellow",lwd = 5)
    text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3, col = 'white')
    mtext(side = 1, line = -2, text = intToUtf8(9658), col = 'green', cex = 2)
    arrows(1,1,1,2, col = 'red', code = 2)
    #mtext(side = 1, '$->', cex = 2, col = 'white', line = -2)
  }else{
    #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = "red",lwd = 8)
    text(1,1.1, strsplit(whichNode, split = '---', fixed = TRUE) %>% unlist() %>% tail(1) %>% as.character(), cex = 1.3)
    mtext(side = 1, line = -2, text = ' ', col = 'red', font = 2, pch = -9668)
    
    #mtext(side = 1, '<-$', cex = 2, col = 'white', line = -2)
  }
}


par(par_default)
par(mai=rep(0.3,4))
node_level = paste("Level",1:ncol(cdt_level))
#text((1:length(node_level)-1)/(length(node_level)-0.4),1.2,node_level,pos = 3)
mtext(node_level,at = (1:length(node_level)-1)/(length(node_level) - 0.8))
}
