#'output.variable.rank.plot
#'
#'output.variable.rank.plot is a ggplot scatter plot with output.variable (mg/kg) on the y axis and chemical rank on the x axis.
#'User can color points by cohort (see "cohort.col" argument) and assign percentiles to be points (see "metrics" argument)
#'
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param output.variable
#'
#'@param metrics argument is a vector of character strings with desired percentiles or metrics. default = c("50%, "95%, "mean)
#'
#'@param cohort.col vector of different cohorts. default = "Total"
#'
#'@return a plot of output.variable vs chemical rank
#'
#'@details A SHEDS run creates an output file for each chemical. The SHEDS estimate "abs.tot.mgkg" is the dependent variable.
#'Chemicals are ranked in ascending order of output.variable on the x axis. The y axis is log transformed. The default color is black with all
#'cohorts represented.'Percentiles must be calculated in SHEDS run. Each data point is a combination of dtxsid, the selected metrics, and
#'the selected cohorts. Data is pulled from allstats output files.
#'
#'@export
#'
#'@import dplyr

output.variable.rank.plot<- function(run.name=specs$run.name, output.variable = "abs.tot.mgkg" , metrics = c("50%", "95%", "mean"), cohort.col = "Total") {
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
  finaldata <-as.data.frame(finaldata)

  finaldata_sub <- finaldata[,c("DTXSID", "Statistic", "Cohort", output.variable)]
  colnames(finaldata_sub) <- c("DTXSID", "Statistic", "Cohort", "output")

  ##Begin Visualization

  #What will the label on the y-axis be?
  if(output.variable == "exp.dermal")       {lab = "exp.dermal (ug/day)"}
  if(output.variable == "exp.ingest")       {lab = "exp.ingest (ug/day)"}
  if(output.variable == "exp.inhal")        {lab = "exp.inhal (ug/m3)"}
  if(output.variable == "dose.inhal")       {lab = "dose.inhal (ug/day)"}
  if(output.variable == "dose.intake")      {lab = "dose.intake (mg/kg/day)"}
  if(output.variable == "abs.dermal.ug")    {lab = "abs.dermal.ug (ug/day)"}
  if(output.variable == "abs.ingest.ug")    {lab = "abs.ingest.ug (ug/day)"}
  if(output.variable == "abs.inhal.ug")     {lab = "abs.inhal.ug (ug/day)"}
  if(output.variable == "abs.tot.ug")       {lab = "abs.tot.ug (ug/day)"}
  if(output.variable == "abs.tot.mgkg")     {lab = "abs.tot.mgkg (mg/kg/day)"}
  if(output.variable == "ddd.mass")         {lab = "ddd.mass (g.day)"}
  if(output.variable == "conc.max.prod.aer"){lab = "conc.max.prod.aer (ug/m3)"}
  if(output.variable == "conc.max.prod.vap"){lab = "conc.max.prod.vap (ug/m3)"}
  if(output.variable == "exp.inhal.indir")  {lab = "exp.inhal.indir (ug/m3)"}
  if(output.variable == "abs.hm.ug")        {lab = "abs.hm.ug (ug/day)"}



  #Create Graphs
  theme_set(theme_bw())

  ##Outdated code when there was a NULL option. Not Total is automatically the default
  if(is.null(cohort.col)){
    fc_null <-finaldata_sub[finaldata_sub$Cohort == "Total",]
    all_plot<-ggplot(fc_null, aes(x=as.numeric(reorder(DTXSID,as.numeric(output)))))+
      scale_y_log10()+
      labs(y = lab, x = "Chemical Rank")+
      geom_point(aes(y=as.numeric(output)+1e-10, shape = Statistic), col="black")+
      scale_shape_discrete(solid=F) + theme_bw()
    cat("\n Creating graph for all cohorts...\n")
    print(all_plot)
  }

  if(!is.null(cohort.col)){
    color_data <- data.frame()
    for(i in 1:length(cohort.col)){
    fc <- finaldata_sub[finaldata_sub$Cohort == cohort.col[i],]
    color_data<-rbind(color_data,fc)}
  cbpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999")
  color_plot<-ggplot(color_data, aes(x=as.numeric(reorder(DTXSID,as.numeric(output)))))+
    geom_point(aes(y=as.numeric(output)+1e-10,  col = Cohort, shape = Statistic))+
    scale_y_log10()+
    labs(y = lab, x = "Chemical Rank")+
    scale_shape_discrete(solid=F)+
    scale_colour_manual(values = cbpalette)

    cat("\n Creating graph for specific cohorts...\n")
    print(color_plot)
  }

}

#' puc.rank.plot is a ggplot scatter plot with desired srcMeans variable on the y axis and chemical rank on the x axis.
#' User can change y axis using output.variable argument. User can either plot the individual product or the higher level puc using the combine argument.
#' Data comes from all_srcMeans output file, so data points are mean output.variable.
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param output.variable argument is a single character string. default = "exp.dermal".
#'
#'@param combine true or false statement that dictates whether to plot each product or combine them so that each data point is a higher level product.
#'
#'@return a plot of output.variable vs chemical rank with a legend for color and shape
#'
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Constant of 1e-10
#' added to y variable for log transformation
#'
#'@export

puc.rank.plot <- function(run.name, output.variable = "exp.dermal", combine = FALSE){
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
  finaldata <- as.data.frame(finaldata)

  finaldata <- finaldata[,c("DTXSID", "pucid", "form", output.variable)]
  colnames(finaldata) <- c("DTXSID", "pucid", "form", "output")

  #Establish Plot labels
  if(output.variable == "exp.dermal")       {lab = "exp.dermal (ug/day)"}
  if(output.variable == "exp.ingest")       {lab = "exp.ingest (ug/day)"}
  if(output.variable == "exp.inhal")        {lab = "exp.inhal (ug/m3)"}
  if(output.variable == "dose.inhal")       {lab = "dose.inhal (ug/day)"}
  if(output.variable == "f.dermal")         {lab = "f.dermal"}
  if(output.variable == "f.ingest")         {lab = "f.ingest"}
  if(output.variable == "f.inhal")          {lab = "f.inhal"}
  if(output.variable == "mean.mass")        {lab = "mean.mass (ug)"}

  options(dplyr.summarise.inform = FALSE)

  if(combine == TRUE){

    plotdata <- finaldata %>%
      mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) %>% #Extract first level of puc id
      group_by(Category, DTXSID, .groups = "drop") %>%
      summarise(output.level = sum(output)) %>%
      select(DTXSID,
             Category,
             output = output.level)
    cat(paste0("\n summing product data for out.variable into higher level puc values \n"))
  }

  if(combine == FALSE){
    #Combine data into PUC groups
    plotdata <- finaldata %>%
      mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) #%>% #Extract first level of puc id
  }


  theme_set(theme_bw())
  cbpalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E411E0")

    #Create graph of output.variable level by PUC
  cat("\n Creating puc plot...\n")
  puc.path.plot <- ggplot(plotdata, aes(x=as.numeric(reorder(DTXSID,as.numeric(output)))))+
      geom_point(aes(y=output+1e-10, color = Category), shape = 1)+
      scale_y_log10()+
      #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
      labs(y = paste0("Mean ", lab), x = "Chemical Rank", color = "PUC")+
      scale_colour_manual(values = cbpalette)
  print(puc.path.plot)
  }
}

#'puc.boxplot is a ggplot boxplot of mean output.variable (mg/kg).
#'User can add shape based on output.variable pathway. Data comes from srcAll output file, so data points are mean output.variable.
#'The user can plot multiple different pathways.
#'
#'@param run.name default = name of last run (in current R session)
#'
#'@param output.variable argument is a concatenated character string. default = NULL plots total output.variable. Can use "exp.total"
#'or "f.total" to plot summation of three available pathways for mean output.variable (g/day) and mean fraction of house chemical mass, respectively.
#'exp.total is calculated as the sum of the exp.dermal, exp.inhal, and exp.ingest pathways. f.total is calculated as
#'
#'@return a boxplot of output.variable with a legend for color and shape
#'
#'@details A SHEDS run creates an output file for each chemical. The y axis is log transformed. Like puc.rank.plot, each product
#'is considered to be a data plot
#'
#'@export

puc.boxplot <- function(run.name, output.variable = c("exp.dermal", "exp.ingest", "exp.inhal")){
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
    mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) #Extract first level of puc id


  #Create graph of output.variable level by PUC
  puc.path.data<-data.frame()
  for(i in 1:length(output.variable)){
      expframe <- puc.level1 %>%
        mutate(exp.pathway = rep(paste0(output.variable[i]), length(nrow))) %>%
        select(DTXSID,
               Category,
               exp.pathway,
               output.variable = paste0(output.variable[i]))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(output.variable)), "\n")
    }

  cat("\n Creating path puc boxplot...\n")

  theme_set(theme_bw())
  plot_exp_path<-ggplot(puc.path.data, aes(x=Category))+
      geom_boxplot(aes(y=output.variable+1e-10))+
      facet_grid(rows = vars(exp.pathway),
                 labeller = labeller(.rows = c("exp.dermal"= "exp.dermal (ug/day)", "exp.ingest"= "exp.ingest (ug/day)", "exp.inhal" = "exp.inhal (ug/m3)",
                                               "dose.inhal" = "dose.inhal (ug/day)", "f.dermal" = "f.dermal","f.ingest" = "f.ingest","f.inhal" = "f.inhal",
                                               "mean.mass" = "mean.mass (ug)"))) +
      labs(y = "", x = "PUC General Category")+
      scale_y_log10()

  print(plot_exp_path)

}

#' puc.dtxsid
#'
#' puc.dtxsid is a ggplot tool that plots mean output.variable (ug/day) for puc ids across specific chemicals.
#'
#' @param run.name default = name of last run (in current R session)
#'
#' @param dtxsid character string of desired chemicals. default = NULL, which plots all of the chemicals. We recommend not using this default for
#' runs with more than 6 chemicals.
#'
#' @param output.variable argument is a concatenated character string of desired output.variables. Can plot as many output.variables as you want at once as long as they are in the file.
#'
#' @return plot of mean output.variable for provided dtxsids
#'
#' @detail Pulls data from the all_srcmeans.csv outputs that are generated by a SHEDS run. The dependent variable, mean output.variable, is the sum of
#' is sum of columns exp.dermal, exp.inhal, and exp.ingest.
#'
#' @example puc.dtxsid(run.name="test", dtxsid=c("DTXSID1029621","DTXSID2044555","DTXSID9021269"))
#'
#' @import ggplot dplyr tidyr stringr
#'
#' @export

puc.dtxsid<- function(run.name=specs$run.name, dtxsid = NULL, output.variable = "exp.dermal"){
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
    for (j in 1:6) {
      filedata      <- read.csv(keepfiles[j])
      filedata      <- filedata[,-1] #remove first row - correct this when I rerun the code, I'm going to do row.name=F for these output files
      DTXSID        <- alldtxsid[j]
      filedata2     <- cbind(DTXSID, filedata)
      setnames(filedata2,names(filedata2)[1],"DTXSID")
      allfiledata[[j]]<-filedata2
      cat(paste0("\n processing chemical ", j, " of 6. If DTXSID argument is left null, this function will not plot more than 6 chemicals."))
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
  options(dplyr.summarise.inform = FALSE)

  plotdata <- finaldata %>%
    mutate(Category = ifelse(pucid == "Total", "Total" , substr(pucid,1,2))) %>% #Extract first level of puc id
    group_by(Category, DTXSID)

  theme_set(theme_bw())


  #Create graph of output.variable level by PUC
  puc.path.data<-data.frame()
    for(i in 1:length(output.variable)){
      expframe <- plotdata %>%
        mutate(exp.pathway = rep(paste0(output.variable[i]), length(nrow))) %>%
        select(DTXSID,
               Category,
               exp.pathway,
               output.variable = paste0(output.variable[i]))
      puc.path.data<-rbind(puc.path.data,expframe)
      cat(paste0("\n data for pathway ", i, " of ", length(output.variable)), "\n")
    }
  puc.path.data.check <- puc.path.data

  puc.barplot <- ggplot(puc.path.data, aes(x = Category, y = output.variable+1e-10))+
      geom_point()+
      facet_grid(rows = vars(exp.pathway), cols = vars(DTXSID),
      labeller = labeller(.rows = c("exp.dermal"= "exp.dermal (ug/day)", "exp.ingest"= "exp.ingest (ug/day)", "exp.inhal" = "exp.inhal (ug/m3)",
                                    "dose.inhal" = "dose.inhal (ug/day)", "f.dermal" = "f.dermal","f.ingest" = "f.ingest","f.inhal" = "f.inhal",
                                    "mean.mass" = "mean.mass (ug)"))) +
      labs(y = "", x = "PUC General Category")+
      scale_y_log10()

  print(puc.barplot)
}
