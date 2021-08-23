#'exposure.rank.plot
#'
#'exposure.rank.plot is a ggplot scatter plot with exposure (mg/kg) on the y axis and chemical rank on the x axis. 
#'User can color points by cohort (the cohort.col argument) and assign percentiles to be points (see "metrics" argument)
#'
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param metrics argument is a concatenated character string. default = c("50%, "95%, "mean)
#'
#'@param cohort.col concatenated character string of different cohorts. default = NULL. Default plots all of the data points without color and 
#'is recommended when there is only one cohort.
#'
#'@return a plot of exposure vs chemical rank
#' 
#'@details A SHEDS run creates an output file for each chemical. The SHEDS estimate "abs.tot.mgkg" is the dependent variable. 
#'Chemicals are ranked in ascending order of exposure on the x axis. The y axis is log transformed. The default color is black with all
#'cohorts represented.'Percentiles must be calculated in SHEDS run. Each data point is a combination of dtxsid, the selected metrics, and
#'the selected cohorts. 
#'
#'@import ggplot dplyr tidyr stringr RColorBrewer
#'
#'@export

exposure.rank.plot<- function(run.name=specs$run.name,metrics = c("50%", "95%", "mean"), cohort.col = NULL) {
  allfiles   <-list.files(path=paste0("output/",run.name), all.files=FALSE, include.dirs=FALSE, full.names=TRUE)
 
   if(length(allfiles)<1){
    stop(paste0(run.name, " is not in the output folder."))
  }
    
  alldtxsid <- list()
  keep  <- rep(FALSE,length(allfiles))
  nchem <- 1
  for (j in 1:length(allfiles)) {
    start <- regexpr("DTXSID",allfiles[j])
    end   <- regexpr("_allstats.csv",allfiles[j])-1
    if(start>4 & end>start) {
      keep[j]<- TRUE
      alldtxsid[nchem] <-substr(allfiles[j],start, end)
      nchem <- nchem+1
    }
  }
  keepfiles <- allfiles[keep]
  allfiledata <- list()
  for (j in 1:length(keepfiles)) {
    filedata<- read.csv(keepfiles[j])
    pracfile2 <- list()
    for(i in 1:length(metrics)){
      datametric<-filedata[filedata$X == metrics[i],]
          if(nrow(datametric)<1){
            stop(paste0("The metric ", metrics[i]," was not calculated by the SHEDS-HT run."))
          }
      names(datametric)[1] <- "Statistic"
      names(datametric)[2] <- "Cohort"
      pracfile2 <- rbind(pracfile2,datametric)
      DTXSID        <- alldtxsid[j]
      alldata2    <- cbind(DTXSID, pracfile2)
      setnames(alldata2,names(alldata2)[1],"DTXSID")
    }
    cat(paste0("\n processing chemical ", j, " of ", length(keepfiles)))
    allfiledata[[j]]<-alldata2
    
  }
  cat("\n Combining data... \n")
  finaldata <- (rbindlist(allfiledata))
  
  
  #Begin Visualization
  #Adding in as.numeric is a clumsy temporary fix
  finaldata<-as.data.frame(finaldata)

  
  if(is.null(cohort.col)){
    all_plot<-ggplot(finaldata, aes(x=as.numeric(reorder(DTXSID,as.numeric(abs.tot.mgkg)))))+
      scale_y_log10()+
      labs(y = "Intake Dose (mg/kg/day)", x = "Chemical Rank")+
      geom_point(aes(y=as.numeric(abs.tot.mgkg)+1e-05, shape = Statistic), col="black")+
      scale_shape_discrete(solid=F) + theme_bw()
    cat("\n Creating black graph...\n")
    print(black_plot)
  }
  
  if(!is.null(cohort.col)){
    color_data <- data.frame()
    for(i in 1:length(cohort.col)){
    fc <- finaldata[finaldata$Cohort == cohort.col[i],]
    color_data<-rbind(color_data,fc)}
  cbpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  color_plot<-ggplot(color_data, aes(x=as.numeric(reorder(DTXSID,as.numeric(abs.tot.mgkg)))))+
    geom_point(aes(y=as.numeric(abs.tot.mgkg)+1e-05,  col = Cohort, shape = Statistic))+
    scale_y_log10()+
    labs(y = "Intake Dose (mg/kg/day)", x = "Chemical Rank")+
    scale_shape_discrete(solid=F)+
    scale_colour_manual(values = cbpalette)
    
    cat("\n Creating color graph...\n")
    print(color_plot)
  }
  
}

#'puc.rank.combine
#'
#'puc.rank.combine is a ggplot scatter plot with exposure (mg/kg) on the y axis and chemical rank on the x axis. 
#'User can add shape based on exposure pathway. Data comes from srcAll output file, so data points are mean exposure. 
#'
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param exposure argument is a concatenated character string. default = c("exp.dermal", "exp.ingest", "exp.inhal"). Can use "exp.total"
#'or "f.total" to plot summation of three available pathways for mean exposure (g/day) and mean fraction of house chemical mass, respectively.
#'exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#'@return a plot of exposure vs chemical rank with a legend for color and shape
#' 
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Difference between this plot and 
#'puc.rank.plot is that this plot combines all product data to one general category One data point is the sum of all 
#'products for that general category and dtxsid. Constant of 1e-05 added to y variable for log transformation.
#'
#'@import ggplot dplyr tidyr stringr RColorBrewer
#'
#'@export

puc.rank.combine <- function(run.name=specs$run.name, exposure = c("exp.dermal", "exp.ingest", "exp.inhal")){
  allfiles   <-list.files(path=paste0("output/",run.name), all.files=FALSE, include.dirs=FALSE, full.names=TRUE)
  
  if(length(allfiles)<1){
    stop(paste0(run.name, " is not in the output folder."))
  }
  
  alldtxsid <- list()
  keep  <- rep(FALSE,length(allfiles))
  nchem <- 1
  for (j in 1:length(allfiles)) {
    start <- regexpr("DTXSID",allfiles[j])
    end   <- regexpr("_all_srcMeans.csv",allfiles[j])-1
    if(start>4 & end>start) {
      keep[j]<- TRUE
      alldtxsid[nchem] <-substr(allfiles[j],start, end)
      nchem <- nchem+1
    }
  }
  keepfiles <- allfiles[keep]
  allfiledata <- list()
  for (j in 1:length(keepfiles)) {
    filedata<- read.csv(keepfiles[j])
    filedata <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
    DTXSID        <- alldtxsid[j]
    filedata2    <- cbind(DTXSID, filedata)
    setnames(filedata2,names(filedata2)[1],"DTXSID")
    allfiledata[[j]]<-filedata2
    cat(paste0("\n processing chemical ", j, " of ", length(keepfiles)))
  }
  cat("\n Combining data... \n")
  finaldata <- (rbindlist(allfiledata))
  
  #Combine data into PUC groups
  puc.level1 <- finaldata %>%
    mutate(puc.level1 = substr(pucid,1,2)) %>% #Extract first level of puc id
    group_by(puc.level1, DTXSID) %>%
    mutate(exp.dermal.level1 = sum(exp.dermal),
           exp.ingest.level1 = sum(exp.ingest),
           exp.inhal.level1 = sum(exp.inhal),
           dose.inhal.level1 = sum(dose.inhal),
           f.dermal.level1 = sum(f.dermal),
           f.ingest.level1 = sum(f.dermal),
           f.inhal.level1 = sum(f.inhal),
           mean.mass.level1 = sum(mean.mass)) %>%
    mutate(exp.total.level1 = sum(exp.dermal.level1, exp.ingest.level1, exp.inhal.level1),
           f.total.level1 = sum(f.dermal.level1, f.ingest.level1, f.inhal.level1)) %>%
    select(DTXSID,
           puc.level1,
           exp.dermal.level1,
           exp.ingest.level1,
           exp.inhal.level1,
           dose.inhal.level1,
           f.dermal.level1,
           f.ingest.level1,
           f.inhal.level1,
           mean.mass.level1,
           exp.total.level1,
           f.total.level1) %>%
    distinct(puc.level1, .keep_all = TRUE)
  
  if(!is.null(exposure)){  
    #Create graph of exposure level by PUC
    puc.path.data<-data.frame()
    for(i in 1:length(exposure)){
      expframe <- puc.level1 %>%
        mutate(exp.pathway = rep(paste0(exposure[i],".level1"), length(nrow))) %>%
        select(DTXSID,
               puc.level1,
               exp.pathway,
               exposure = paste0(exposure[i],".level1"))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(exposure)), "\n")
    }
    cat("\n Creating path puc plot...\n")
    puc.path.plot <- ggplot(puc.path.data, aes(x=as.numeric(reorder(DTXSID,as.numeric(exposure)))))+
      geom_point(aes(y=exposure+1e-5, color = puc.level1, shape = exp.pathway))+
      scale_y_log10()+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      labs(y = "Mean Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F)
    print(puc.path.plot)
  }
  
  #Create graph of exposure level by PUC
  if(is.null(exposure)){
    cat("\n Creating null plot...\n")
    plot_base<-ggplot(puc.level1, aes(x=as.numeric(reorder(DTXSID,as.numeric(exp.total.level1)))))+
      geom_point(aes(y=exp.total.level1+1e-5, color = puc.level1))+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_y_log10()+
      labs(y = "Total Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F)
    print(plot_base)
  }
}

#'puc.rank.plot is a ggplot scatter plot with exposure (mg/kg) on the y axis and chemical rank on the x axis. 
#'User can add shape based on exposure pathway. Data comes from srcAll output file, so data points are mean exposure. 
#'The difference between this plot and puc.rank.combine is that this plots each product.
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param exposure argument is a concatenated character string. default = c("exp.dermal", "exp.ingest", "exp.inhal"). Can use "exp.total"
#'or "f.total" to plot summation of three available pathways for mean exposure (g/day) and mean fraction of house chemical mass, respectively.
#'exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#'@return a plot of exposure vs chemical rank with a legend for color and shape
#' 
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Constant of 1e-05
#' added to y variable for log transformation
#' 
#'@import ggplot dplyr tidyr stringr RColorBrewer
#' 
#'@export

puc.rank.plot <- function(run.name, exposure = NULL){
  allfiles   <-list.files(path=paste0("output/",run.name), all.files=FALSE, include.dirs=FALSE, full.names=TRUE)
  
  if(length(allfiles)<1){
    stop(paste0(run.name, " is not in the output folder."))
  }
  
  alldtxsid <- list()
  keep  <- rep(FALSE,length(allfiles))
  nchem <- 1
  for (j in 1:length(allfiles)) {
    start <- regexpr("DTXSID",allfiles[j])
    end   <- regexpr("_all_srcMeans.csv",allfiles[j])-1
    if(start>4 & end>start) {
      keep[j]<- TRUE
      alldtxsid[nchem] <-substr(allfiles[j],start, end)
      nchem <- nchem+1
    }
  }
  keepfiles <- allfiles[keep]
  allfiledata <- list()
  for (j in 1:length(keepfiles)) {
    filedata<- read.csv(keepfiles[j])
    filedata <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
    DTXSID        <- alldtxsid[j]
    filedata2    <- cbind(DTXSID, filedata)
    setnames(filedata2,names(filedata2)[1],"DTXSID")
    allfiledata[[j]]<-filedata2
    cat(paste0("\n processing chemical ", j, " of ", length(keepfiles)))
  }
  cat("\n Combining data... \n")
  finaldata <<- (rbindlist(allfiledata))
  
  #Combine data into PUC groups
  puc.level1 <- finaldata %>%
    mutate(Category = substr(pucid,1,2)) %>% #Extract first level of puc id
    group_by(Category, DTXSID) %>%
    mutate(exp.total = sum(exp.dermal, exp.ingest, exp.inhal),
           f.total = sum(f.dermal, f.ingest, f.inhal)) %>%
    distinct(Category, .keep_all = TRUE)
  
  if(!is.null(exposure)){  
    #Create graph of exposure level by PUC
    puc.path.data<-data.frame()
    for(i in 1:length(exposure)){
      expframe <- puc.level1 %>%
        mutate(exp.pathway = rep(paste0(exposure[i]), length(nrow))) %>%
        select(DTXSID,
               Category,
               exp.pathway,
               exposure = paste0(exposure[i]))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(exposure)), "\n")
    }
    cat("\n Creating path puc plot...\n")
    puc.path.plot <- ggplot(puc.path.data, aes(x=as.numeric(reorder(DTXSID,as.numeric(exposure)))))+
      geom_point(aes(y=exposure+1e-05, color = Category, shape = exp.pathway))+
      scale_y_log10()+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      labs(y = "Mean Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F)
    print(puc.path.plot)
  }
  
  #Create graph of exposure level by PUC
  if(is.null(exposure)){
    cat("\n Creating null plot...\n")
    plot_base<-ggplot(puc.level1, aes(x=as.numeric(reorder(DTXSID,as.numeric(exp.total)))))+
      geom_point(aes(y=exp.total+1e-05, color = Category))+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_y_log10()+
      labs(y = "Total Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F)
    print(plot_base)
  }
}
puc.rank.plot(run.name = "test", exposure = c("exp.dermal"))

#'puc.rank.boxplot is a ggplot boxplot of mean exposure (mg/kg) vs  
#'User can add shape based on exposure pathway. Data comes from srcAll output file, so data points are mean exposure. 
#'The difference between this plot and puc.rank.combine is that this plots each product.
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param exposure argument is a concatenated character string. default = NULL plots total exposure. Can use "exp.total"
#'or "f.total" to plot summation of three available pathways for mean exposure (g/day) and mean fraction of house chemical mass, respectively.
#'exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#'@return a boxplot of exposure with a legend for color and shape
#' 
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Like puc.rank.plot, each product
#'is considered to be a data plot
#'
#'@import ggplot dplyr tidyr stringr RColorBrewer
#'
#'@export

puc.data.boxplot <- function(run.name, exposure = NULL){
  allfiles   <-list.files(path=paste0("output/",run.name), all.files=FALSE, include.dirs=FALSE, full.names=TRUE)
  
  if(length(allfiles)<1){
    stop(paste0(run.name, " is not in the output folder."))
  }
  
  alldtxsid <- list()
  keep  <- rep(FALSE,length(allfiles))
  nchem <- 1
  for (j in 1:length(allfiles)) {
    start <- regexpr("DTXSID",allfiles[j])
    end   <- regexpr("_all_srcMeans.csv",allfiles[j])-1
    if(start>4 & end>start) {
      keep[j]<- TRUE
      alldtxsid[nchem] <-substr(allfiles[j],start, end)
      nchem <- nchem+1
    }
  }
  keepfiles <- allfiles[keep]
  allfiledata <- list()
  for (j in 1:length(keepfiles)) {
    filedata<- read.csv(keepfiles[j])
    filedata <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
    DTXSID        <- alldtxsid[j]
    filedata2    <- cbind(DTXSID, filedata)
    setnames(filedata2,names(filedata2)[1],"DTXSID")
    allfiledata[[j]]<-filedata2
    cat(paste0("\n processing chemical ", j, " of ", length(keepfiles)))
  }
  cat("\n Combining data... \n")
  finaldata <- (rbindlist(allfiledata))
  
  
  #Combine data into PUC groups
  puc.level1 <- finaldata %>%
    mutate(Category = substr(pucid,1,2)) %>% #Extract first level of puc id
    group_by(Category, DTXSID) %>%
    mutate(exp.total = sum(exp.dermal, exp.ingest, exp.inhal),
           f.total = sum(f.dermal, f.ingest, f.inhal)) %>%
    distinct(Category, .keep_all = TRUE)
  
  #Create graph of exposure level by PUC
  if(is.null(exposure)){
    cat("\n Creating null plot...\n")
    plot_base<-ggplot(puc.level1, aes(x=Category))+
      geom_boxplot(aes(y=exp.total+1e-05))+
      labs(y = "Total Mean Exposure (mg/kg/day)", x = "PUC General Category")+
      scale_y_log10()
    print(plot_base)
  }
  
  if(!is.null(exposure)){  
    #Create graph of exposure level by PUC
    puc.path.data<-data.frame()
    for(i in 1:length(exposure)){
      expframe <- puc.level1 %>%
        mutate(exp.pathway = rep(paste0(exposure[i]), length(nrow))) %>%
        select(DTXSID,
               Category,
               exp.pathway,
               exposure = paste0(exposure[i]))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(exposure)), "\n")
    }
    cat("\n Creating path puc boxplot...\n")
    puc.path.data<<-puc.path.data
    plot_exp_path<-ggplot(puc.path.data, aes(x=Category))+
      geom_boxplot(aes(y=exposure+1e-05))+
      facet_grid(rows = vars(exp.pathway))+
      labs(y = "Total Mean Exposure (mg/kg/day)", x = "PUC General Category")+
      scale_y_log10()
    print(plot_exp_path)
  }

}
puc.data.boxplot(run.name = "test", exposure = NULL)
puc.data.boxplot(run.name = "test", exposure = c("exp.dermal", "exp.inhal", "exp.total"))

#' puc.dtxsid.barchart
#'
#' puc.dtxsid.barchart is a ggplot tool that creates a bar chart of mean exposure (ug/day) for puc ids across specific chemicals.
#' 
#' @param run.name default = name of last run (in current R session)
#' 
#' @param dtxsid character string of desired chemicals. default = NULL, which plots all of the chemicals. We recommend not using this default for 
#' runs with more than 6 chemicals. 
#' 
#' @return boxplot of mean exposure for provided dtxsids
#' 
#' @detail Pulls data from the all_srcmeans.csv outputs that are generated by a SHEDS run. The dependent variable, mean exposure, is the sum of 
#' is sum of columns exp.dermal, exp.inhal, and exp.ingest.
#' 
#' @example puc.dtxsid.barchart(run.name="test", dtxsid=c("DTXSID1029621","DTXSID2044555","DTXSID9021269")) 
#' 
#' @import ggplot dplyr tidyr stringr RColorBrewer
#' 
#' @export

puc.dtxsid.barchart<- function(run.name=specs$run.name, dtxsid = NULL){
  if(is.null(dtxsid)){
    allfiles   <-list.files(path=paste0("output/",run.name), all.files=FALSE, include.dirs=FALSE, full.names=TRUE)
    if(length(allfiles)<1){
    stop(paste0(run.name, " is not in the output folder."))
  }
    alldtxsid <- list()
    keep  <- rep(FALSE,length(allfiles))
    nchem <- 1
    for (j in 1:length(allfiles)) {
      start <- regexpr("DTXSID",allfiles[j])
      end   <- regexpr("_all_srcMeans.csv",allfiles[j])-1
      if(start>4 & end>start) {
        keep[j]<- TRUE
        alldtxsid[nchem] <-substr(allfiles[j],start, end)
        nchem <- nchem+1
      }
      }
    keepfiles <- allfiles[keep]
    allfiledata <- list()
    for (j in 1:length(keepfiles)) {
      filedata      <- read.csv(keepfiles[j])
      filedata      <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
      DTXSID        <- alldtxsid[j]
      filedata2     <- cbind(DTXSID, filedata)
      setnames(filedata2,names(filedata2)[1],"DTXSID")
      allfiledata[[j]]<-filedata2
      cat(paste0("\n processing chemical ", j, " of ", length(keepfiles)))
      }
    cat("\n Combining data... \n")
    finaldata <- (rbindlist(allfiledata))
    }
  
  if(!is.null(dtxsid)){
    allfiledata <- list()
    for(i in 1:length(dtxsid)) {
      filedata      <- read.csv(paste0("output/", run.name, "/", dtxsid[i],"_all_srcMeans.csv"))
      filedata      <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
      DTXSID        <- dtxsid[i]
      filedata2     <- cbind(DTXSID, filedata)
      setnames(filedata2,names(filedata2)[1],"DTXSID")
      allfiledata[[i]]<-filedata2
      cat(paste0("\n processing chemical ", i, " of ", length(dtxsid)))
      }
    cat("\n Combining data... \n")
    finaldata <- (rbindlist(allfiledata))
  }
  
#Combine data into PUC groups
  puc.level1 <- finaldata %>%
    mutate(puc.level1 = substr(pucid,1,2)) %>% #Extract first level of puc id
    filter(puc.level1 != "To") %>%
    group_by(puc.level1, DTXSID) %>%
    mutate(exp.dermal.level1 = sum(exp.dermal),
           exp.ingest.level1 = sum(exp.ingest),
           exp.inhal.level1 = sum(exp.inhal),
           dose.inhal.level1 = sum(dose.inhal),
           f.dermal.level1 = sum(f.dermal),
           f.ingest.level1 = sum(f.dermal),
           f.inhal.level1 = sum(f.inhal),
           mean.mass.level1 = sum(mean.mass)) %>%
    mutate(exp.total.level1 = sum(exp.dermal.level1, exp.ingest.level1, exp.inhal.level1),
           f.total.level1 = sum(f.dermal.level1, f.ingest.level1, f.inhal.level1)) %>%
    select(DTXSID,
           pucid = puc.level1,
           exp.dermal = exp.dermal.level1,
           exp.ingest = exp.ingest.level1,
           exp.inhal = exp.inhal.level1,
           dose.inhal = dose.inhal.level1,
           f.dermal = f.dermal.level1,
           f.ingest = f.ingest.level1,
           f.inhal = f.inhal.level1,
           mean.mass = mean.mass.level1,
           exp.total = exp.total.level1,
           f.total = f.total.level1) %>%
    distinct(pucid, .keep_all = TRUE)
  
  #Create Plot 
  puc.barplot <- ggplot(puc.level1, aes(x = pucid, y = exp.total))+
    geom_bar(stat = "identity")+
    facet_grid(rows = vars(DTXSID))+
    labs(y = "Mean Exposure (ug/day)", x = "PUC General Category")+
    scale_y_log10()
  print(puc.barplot)
  }