#'exposure.rank.plot
#'
#'exposure.rank.plot is a ggplot scatter plot with exposure (mg/kg) on the y axis and chemical rank on the x axis. 
#'User can color points by cohort (see "cohort.col" argument) and assign percentiles to be points (see "metrics" argument)
#'
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param metrics argument is a vector of character strings with desired percentiles or metrics. default = c("50%, "95%, "mean)
#'
#'@param cohort.col vector of different cohorts. default = NULL. Default will plot all of the data points without color and it is only
#'recommended when there is only one cohort.
#'
#'@return a plot of exposure vs chemical rank
#' 
#'@details A SHEDS run creates an output file for each chemical. The SHEDS estimate "abs.tot.mgkg" is the dependent variable. 
#'Chemicals are ranked in ascending order of exposure on the x axis. The y axis is log transformed. The default color is black with all
#'cohorts represented.'Percentiles must be calculated in SHEDS run. Each data point is a combination of dtxsid, the selected metrics, and
#'the selected cohorts. Data is pulled from allstats output files.
#'
#'@export
#'
#'@import dplyr

exposure.rank.plot<- function(run.name=specs$run.name, metrics = c("50%", "95%", "mean"), cohort.col = NULL) {
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
  
  theme_set(theme_bw())
  
  if(is.null(cohort.col)){
    all_plot<-ggplot(finaldata, aes(x=as.numeric(reorder(DTXSID,as.numeric(abs.tot.mgkg)))))+
      scale_y_log10()+
      labs(y = "Intake Dose (mg/kg/day)", x = "Chemical Rank")+
      geom_point(aes(y=as.numeric(abs.tot.mgkg)+1e-10, shape = Statistic), col="black")+
      scale_shape_discrete(solid=F) + theme_bw()
    cat("\n Creating graph for all cohorts...\n")
    print(all_plot)
  }
  
  if(!is.null(cohort.col)){
    color_data <- data.frame()
    for(i in 1:length(cohort.col)){
    fc <- finaldata[finaldata$Cohort == cohort.col[i],]
    color_data<-rbind(color_data,fc)}
  cbpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  color_plot<-ggplot(color_data, aes(x=as.numeric(reorder(DTXSID,as.numeric(abs.tot.mgkg)))))+
    geom_point(aes(y=as.numeric(abs.tot.mgkg)+1e-10,  col = Cohort, shape = Statistic))+
    scale_y_log10()+
    labs(y = "Intake Dose (mg/kg/day)", x = "Chemical Rank")+
    scale_shape_discrete(solid=F)+
    scale_colour_manual(values = cbpalette)
    
    cat("\n Creating graph for specific cohorts...\n")
    print(color_plot)
  }
  
}

#' puc.rank.plot is a ggplot scatter plot with exposure (mg/kg) on the y axis and chemical rank on the x axis. 
#' User can add shape based on exposure pathway using exposure argument. User can either plot the individual product or the higher level puc using the combine argument.
#' Data comes from all_srcMeans output file, so data points are mean exposure. 
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param exposure argument is a concatenated character string. default = c("exp.dermal", "exp.ingest", "exp.inhal"). Can use "exp.total"
#'or "f.total" to plot summation of three available pathways for mean exposure (g/day) and mean fraction of house chemical mass, respectively.
#'exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#'@param combine true or false statement that dictates whether to plot each product or combine them so that each data point is a higher level product.
#'
#'@return a plot of exposure vs chemical rank with a legend for color and shape
#' 
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Constant of 1e-10
#' added to y variable for log transformation
#'
#' 
#'@export

puc.rank.plot <- function(run.name, exposure = NULL, combine = FALSE){
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
  
  options(dplyr.summarise.inform = FALSE)
  
  if(combine == TRUE){
    
    plotdata <- finaldata %>%
      mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) %>% #Extract first level of puc id
      group_by(Category, DTXSID, .groups = "drop") %>%
      summarise(exp.dermal.level1 = sum(exp.dermal),
             exp.ingest.level1 = sum(exp.ingest),
             exp.inhal.level1 = sum(exp.inhal),
             dose.inhal.level1 = sum(dose.inhal),
             f.dermal.level1 = sum(f.dermal),
             f.ingest.level1 = sum(f.ingest),
             f.inhal.level1 = sum(f.inhal),
             mean.mass.level1 = sum(mean.mass)) %>%
      mutate(exp.total.level1 = sum(exp.dermal.level1, exp.ingest.level1, exp.inhal.level1),
             f.total.level1 = sum(f.dermal.level1, f.ingest.level1, f.inhal.level1)) %>%
      select(DTXSID,
             Category,
             exp.dermal = exp.dermal.level1,
             exp.ingest = exp.ingest.level1,
             exp.inhal = exp.inhal.level1,
             dose.inhal = dose.inhal.level1,
             f.dermal = f.dermal.level1,
             f.ingest = f.ingest.level1,
             f.inhal = f.inhal.level1,
             mean.mass = mean.mass.level1,
             exp.total = exp.total.level1,
             f.total = f.total.level1)
    cat(paste0("\n summing product exposure data into higher level puc values \n"))
  }
  
  if(combine == FALSE){
    #Combine data into PUC groups
    plotdata <- finaldata %>%
      mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) #%>% #Extract first level of puc id
      #group_by(Category, DTXSID) %>%
      #mutate(exp.total = sum(exp.dermal, exp.ingest, exp.inhal),
             #f.total = sum(f.dermal, f.ingest, f.inhal))
  }
  
  
  theme_set(theme_bw())
  cbpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E411E0")
  
  
  if(!is.null(exposure)){  
    #Create graph of exposure level by PUC
    puc.path.data<-data.frame()
    for(i in 1:length(exposure)){
      expframe <- plotdata %>%
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
      geom_point(aes(y=exposure+1e-10, color = Category, shape = exp.pathway))+
      scale_y_log10()+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      labs(y = "Mean Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F) +
      scale_colour_manual(values = cbpalette)
    print(puc.path.plot)
  }
  
  #Create graph of exposure level by PUC
  if(is.null(exposure)){
    cat("\n Creating null plot...\n")
    plot_base<-ggplot(plotdata, aes(x=as.numeric(reorder(DTXSID,as.numeric(exp.total)))))+
      geom_point(aes(y=exp.total+1e-10, color = Category))+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      scale_y_log10()+
      labs(y = "Total Exposure (mg/kg/day)", x = "Chemical Rank", color = "PUC")+
      scale_shape_discrete(solid=F)+
      scale_colour_manual(values = cbpalette)
    print(plot_base)
  }
}

#'puc.rank.boxplot is a ggplot boxplot of mean exposure (mg/kg).  
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
    mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) %>% #Extract first level of puc id
    group_by(Category, DTXSID) %>%
    mutate(exp.total = sum(exp.dermal, exp.ingest, exp.inhal),
           f.total = sum(f.dermal, f.ingest, f.inhal)) 
  
  
  #Create graph of exposure level by PUC
  if(is.null(exposure)){
    cat("\n Creating null plot...\n")
    plot_base<-ggplot(puc.level1, aes(x=Category))+
      geom_boxplot(aes(y=exp.total+1e-10))+
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
    
    theme_set(theme_bw())
    plot_exp_path<-ggplot(puc.path.data, aes(x=Category))+
      geom_boxplot(aes(y=exposure+1e-10))+
      facet_grid(rows = vars(exp.pathway))+
      labs(y = "Total Mean Exposure (mg/kg/day)", x = "PUC General Category")+
      scale_y_log10()
    print(plot_exp_path)
  }

}

#' puc.dtxsid
#'
#' puc.dtxsid is a ggplot tool that plots mean exposure (ug/day) for puc ids across specific chemicals.
#' 
#' @param run.name default = name of last run (in current R session)
#' 
#' @param dtxsid character string of desired chemicals. default = NULL, which plots all of the chemicals. We recommend not using this default for 
#' runs with more than 6 chemicals. 
#' 
#' @param exposure argument is a concatenated character string. default = NULL plots total exposure. Can use "exp.total"
#' or "f.total" to plot summation of three available pathways for mean exposure (g/day) and mean fraction of house chemical mass, respectively.
#' exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#' @return plot of mean exposure for provided dtxsids
#' 
#' @detail Pulls data from the all_srcmeans.csv outputs that are generated by a SHEDS run. The dependent variable, mean exposure, is the sum of 
#' is sum of columns exp.dermal, exp.inhal, and exp.ingest.
#' 
#' @example puc.dtxsid(run.name="test", dtxsid=c("DTXSID1029621","DTXSID2044555","DTXSID9021269")) 
#' 
#' @import ggplot dplyr tidyr stringr
#' 
#' @export

puc.dtxsid<- function(run.name=specs$run.name, dtxsid = NULL, exposure = NULL){
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
    finaldata <<- (rbindlist(allfiledata))
  }
  
  
  #Combine data into PUC groups
  options(dplyr.summarise.inform = FALSE)
  
  plotdata <- finaldata %>%
    mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) %>% #Extract first level of puc id
    group_by(Category, DTXSID, .groups = "drop") %>%
    summarise(exp.dermal.level1 = sum(exp.dermal),
              exp.ingest.level1 = sum(exp.ingest),
              exp.inhal.level1 = sum(exp.inhal),
              dose.inhal.level1 = sum(dose.inhal),
              f.dermal.level1 = sum(f.dermal),
              f.ingest.level1 = sum(f.ingest),
              f.inhal.level1 = sum(f.inhal),
              mean.mass.level1 = sum(mean.mass)) %>%
    mutate(exp.total.level1 = sum(exp.dermal.level1, exp.ingest.level1, exp.inhal.level1),
           f.total.level1 = sum(f.dermal.level1, f.ingest.level1, f.inhal.level1)) %>%
    select(DTXSID,
           Category,
           exp.dermal = exp.dermal.level1,
           exp.ingest = exp.ingest.level1,
           exp.inhal = exp.inhal.level1,
           dose.inhal = dose.inhal.level1,
           f.dermal = f.dermal.level1,
           f.ingest = f.ingest.level1,
           f.inhal = f.inhal.level1,
           mean.mass = mean.mass.level1,
           exp.total = exp.total.level1,
           f.total = f.total.level1)
  cat("\n data combined \n")
  
  theme_set(theme_bw()) 
  
  if(is.null(exposure)){
    
    #Create Plot 
    puc.barplot <- ggplot(plotdata, aes(x = Category, y = exp.total+1e-10))+
      geom_point()+
      facet_grid(rows = vars(DTXSID))+
      labs(y = "Mean Exposure (ug/day)", x = "PUC General Category")+
      scale_y_log10()
    print(puc.barplot)
  }
  
  if(!is.null(exposure)){  
    #Create graph of exposure level by PUC
    puc.path.data<-data.frame()
    for(i in 1:length(exposure)){
      expframe <- plotdata %>%
        mutate(exp.pathway = rep(paste0(exposure[i]), length(nrow))) %>%
        select(DTXSID,
               Category,
               exp.pathway,
               exposure = paste0(exposure[i]))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(exposure)), "\n")
    }
    
    puc.barplot <- ggplot(puc.path.data, aes(x = Category, y = exposure+1e-10))+
      geom_point()+
      facet_grid(rows = vars(exp.pathway), cols = vars(DTXSID))+
      labs(y = "Mean Exposure (ug/day)", x = "PUC General Category")+
      scale_y_log10()
    
    print(puc.barplot)
  }
}
