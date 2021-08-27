## The goal of this code is to create source_chem_cpdat input file for the 2021 version of SHEDS-HT. This version of the file uses
## the 20210128 Factotum bulk download and the packaged dplyr, tidyr, and stringr.
##
## Raw weight fractions can be cleaned and added to the dataset. Forms that are missing will be replaced by their default, and ";" 
## in form will be separated onto different lines. 
##
##  Inputs Files:
##      chem_data = chemical_dictionary_20210128.csv. This is used to add DTXSID and other chemical information to source_chem
##      puc_data = PUC_dictionary_20210128.csv. This contains all of the descriptive PUC data that we need to identify PUCS
##      weight_data = product_composition_data_20210128.csv. This adds weight and composition data
##      form_dat = PUC_Defaults.csv. I pulled this data from the SHEDS paper. If form is not in attribute_name from weight_data, I will replace the NA with the default form given here
##
## As of 3/24/2021, I added in document_id to identify the source of the different product/form matches. This will be removed
## As of 6/15/2021, I added in product_id to identify the distinct number of products that will be removed (not to be confused with product.id that is used by SHEDS)
## As of 6/23/2021, used updated the documents to be the most recent factotum download to fix attribute bug.
##          Also adding in chemical_id so that I can reference original release
## As of 8/24/2021, I add prod_type and prod_title to help identify duplicates. Duplicates is defined as any product numbers for which the chemicals and SET of weight fractions is the same
## As of 8/26/2021, I use the 8/25/2021 download. I also filter out duplicates. 

## ------------------------------- CODE SETUP ----------------------------------------------------------------------
library(dplyr); library(tidyr); library(stringr); library(janitor)
##This is the reference file that Graham sent over
source_chem_original <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/SHEDS Project/SHEDSHTRPackage-master/Inputs/source_chem_products.csv")
source_chem_example <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/SHEDS Project/SHEDSDevel-ICF-DEVEL/Inputs/source_chem_5.csv" ,header=T)

#Read in the various factotum documents
chem_data.august<- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/Factotum Download/CPDat_08252021/chemical_dictionary_2021-08-24.csv",header=T)
puc_data.august<- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/Factotum Download/CPDat_08252021/PUC_dictionary_2021-08-24.csv", header=T)
weight_data.august<- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/Factotum Download/CPDat_08252021/product_composition_data_2021-08-24.csv", header=T)
#weight_data.old<- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/Factotum Download/product_composition_data_20210128.csv", header=T)

form_dat <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/Factotum Download/PUC_Defaults.csv", header = T) #For importing form defaults if form is not reported

#Check files to make sure I have the right data and nothing looks bizarre
str(chem_data)
str(puc_data)
str(weight_data)
str(form_dat)

## -------------------------------- STEP 1: Clean weight fraction data ---------------------------------------------

##Adding in row.id number in weight_data to make sure I'm not cleaning the same data twice
weight_data.august <- weight_data.august %>% mutate(row.id = rownames(weight_data.august))

#Start with minimum and maximum weight fractions. I only filter by minimum weight fraction here because both min AND max are 
#necessary, and I will filter for missing max fractions later
unclean.august <- weight_data.august %>%
  filter(!is.na(raw_min_comp) & is.na(weight_data.august$clean_min_wf))#If raw min comp is present and clean wf is not
##Does the min/max weight fraction have any puncuation other than decimal, like >,<, or =? If yes, remove that data
uncleanmin.august <- unclean.august %>%
  filter(str_detect(raw_min_comp, ">")) %>%
  filter(!str_detect(raw_max_comp, ">")) %>%
  mutate(raw_min_comp = str_replace(raw_min_comp, ">",""),
         raw_min_comp = str_replace(raw_min_comp, "=",""), 
         raw_min_comp = str_replace(raw_min_comp," ",""),
         raw_min_comp = str_replace(raw_min_comp, "\\.0$",""),
         raw_max_comp = str_replace(raw_max_comp, "<",""),
         raw_max_comp = str_replace(raw_max_comp, "=", ""),
         raw_max_comp = str_replace(raw_max_comp, " ",""),
         raw_max_comp = str_replace(raw_max_comp, "\\.0$",""),
         raw_max_comp = str_replace(raw_max_comp, "\\-", "")) %>% #This line targets "5-0". Min is 25, so I'm confident it's 50
  #Create a temporary column to store temporary values. Add a .0 two any min or max wf with 2 or 1 value. 
  #Will check later to ensure min is still smaller than max, and something like 20 should not have been 2.0
  #Will also check that min = 1 should be 
  mutate(temp_min_comp = ifelse(nchar(raw_min_comp) == 2, paste0(".", raw_min_comp),
                                (ifelse(nchar(raw_min_comp)==1, paste0(".0",raw_min_comp), NA))), 
         temp_max_comp = ifelse(nchar(raw_max_comp) == 2, paste0(".", raw_max_comp),
                                (ifelse(nchar(raw_max_comp)==1, paste0(".0",raw_max_comp), NA)))) %>%
  mutate(clean_min_wf = temp_min_comp,
         clean_max_wf = temp_max_comp) %>%
  select(-temp_min_comp, -temp_max_comp)
##Checks
table(uncleanmin.august$clean_min_wf)
table(uncleanmin.august$clean_max_wf)


table(uncleanmin.august$temp_max_comp)
table(uncleanmin.august$temp_min_comp)

#Does the min/max weight fraction NOT have any punctuaion? If so, you haven't dealt with it yet. Add .0 
unclean.0.august <- unclean.august %>% 
  filter(!str_detect(raw_min_comp, "<"))%>%
  filter(!str_detect(raw_min_comp, ">"))%>%
  filter(!str_detect(raw_max_comp, "<"))%>%
  filter(!str_detect(raw_max_comp, "\\)"))%>%
  filter(!str_detect(raw_max_comp, "\\%")) %>%
  filter(!str_detect(raw_min_comp, "[:alpha:]")) %>%
  filter(!str_detect(raw_max_comp, "[:alpha:]")) %>%
  filter(!str_detect(raw_min_comp, "\\+")) %>%
  filter(!str_detect(raw_max_comp,  "\\+")) %>%
  filter(!str_detect(raw_min_comp, "\\_")) %>%
  filter(!str_detect(raw_min_comp, "\\.")) %>%
  #filter(str_detect(raw_min_comp, "\\.0$")) %>%
  #mutate(raw_min_comp = str_replace(raw_min_comp, "\\.0$",""),
  #raw_max_comp = str_replace(raw_max_comp, "\\.0$",""))%>%
  mutate(        #raw_max_comp = str_replace(raw_max_comp, "8o", "80") #Fairly confident in this one as min is 75%>% ->not worth adding in the one datapoint, will only make the future pipeline more difficult
    temp_min_comp = ifelse(
      nchar(raw_min_comp) == 2, paste0(".", raw_min_comp),
                                (ifelse
                                 (nchar(raw_min_comp)==1, paste0(".0",raw_min_comp), 
                                        NA))),
    temp_max_comp = ifelse(
      nchar(raw_max_comp) == 2, paste0(".", raw_max_comp),
                                (ifelse
                                 (nchar(raw_max_comp)==1, paste0(".0",raw_max_comp),
                                   NA)))) %>%
mutate(clean_min_wf = temp_min_comp,
  clean_max_wf = temp_max_comp) %>%
  select(-temp_min_comp, #Remove temporary storage columns
         -temp_max_comp) %>%
  filter(clean_min_wf <= clean_max_wf) #ONLY keep values where min is less than max. If it's not, filter out!
#%>%mutate(temp_max_comp = str_replace(raw_max_comp, "\\.2.","20"))

#Checks
table(unclean.0.august$raw_min_com)
table(unclean.0.august$temp_min_compa)
table(unclean.0.august$clean_min_wf)


table(unclean.0.august$raw_max_com)
table(unclean.0.august$temp_max_compa)
table(unclean.0.august$clean_max_wf)


##Now look at the behavior of central weights
uncleancentral.august <- weight_data.august %>%
  filter(is.na(raw_min_comp) & is.na(clean_min_wf) & is.na(clean_max_wf) &
           is.na(clean_central_wf) &is.na(raw_max_comp)& !is.na(raw_central_comp)) %>%
  filter(!str_detect(raw_central_comp, "k")) %>%
  filter(!str_detect(raw_central_comp, "[:punct:]")) %>%
  filter(!str_detect(raw_central_comp, "[:alpha:]")) %>%
  filter(!str_detect(raw_central_comp, "<")) %>%
  filter(!str_detect(raw_central_comp, ">")) %>%
  filter(!str_detect(raw_central_comp, "~")) %>%
  mutate(temp_central_comp = ifelse(nchar(raw_central_comp) == 2, paste0(".", raw_central_comp), 
                                    ifelse(nchar(raw_central_comp) == 1, paste0(".0", raw_central_comp), NA)))%>%
  mutate(clean_central_wf = temp_central_comp) %>%
  select(-temp_central_comp)

table(uncleancentral.august$raw_central_comp[which(nchar(uncleancentral.august$raw_central_comp)== 2)])
table(uncleancentral.august$raw_central_comp[which(nchar(uncleancentral.august$raw_central_comp)== 1)])
table(uncleancentral.august$temp_central_comp)
table(uncleancentral.august$clean_central_wf)

##Combine clean weight_data to make weight_clean data frame
weight_point_sub<- weight_data.august %>%
  filter(is.na(clean_min_wf) & is.na(clean_max_wf) & !is.na(clean_central_wf))
weight_uniform_sub<- weight_data.august %>% 
  filter(!is.na(clean_min_wf) & !is.na(clean_max_wf))

weight_clean <- rbind(weight_point_sub, weight_uniform_sub, unclean.0.august, uncleancentral.august)

stop()

## --------------------------------- STEP 2: Combine Factotum tables and clean data to create base cpdat files------------------------------------------------

#Begin to create source chem
cpdat_pi<-chem_data.august %>%
  left_join(weight_clean, by = c("chemical_id"="chemical_id"))%>% #Clean weight data from previous step
  left_join(puc_data.august, by = c("puc_id" = "puc_id")) %>%
  left_join(form_dat, by = c("puc_code" ="PUCID")) %>%
  mutate(source.type = rep(paste("product"), length(nrow)),    #All same based on source_chem
         min.age = rep(0, length(nrow)),                       #All same based on source_chem
         max.age = rep(99, length(nrow)),                      #All same based on source_chem
         gender = rep(paste("B"), length(nrow)),               #All same based on source_chem
         pr.chem = rep(1, length(nrow)),
         form = ifelse(is.na(attribute_name), Default.Form, attribute_name)) %>% #If attribute name is not available, replace with default form from SHEDS documentation
  separate_rows(form, sep = ";") %>% #Some attribute_names have a ";", so separate those out
  select(source.type,
         pucid = puc_code,
         dtxsid = DTXSID,
         form,
         min.age,
         max.age,
         gender,
         pr.chem,
         raw_min_comp,  #To be dropped later, but or later data cleaning
         raw_central_comp, #To be dropped later, but needs to be kept for later data cleaning
         raw_max_comp, #To be dropped later, but needs to be kept for later data cleaning
         clean_min_wf,
         clean_central_wf,
         clean_max_wf,
         document_id, #Added temporarily to track down weird puc/form matches. Remove before final product! 
         product_id, #Added temporarily to track down weird puc/form matches. Remove before final product!   
         chemical_id, #Added temporarily to match documents to official factotum download
         prod_title, #Added temporarily to check for duplicates
         prod_type) #Added temporarily

## --------------------------------- STEP 3: Use clean weight fractions to fill in par1, par2, and dist form -----------------------------------------------

## If clean weight fraction is available for min and max, use a uniform function  for dist.type and fill par1 with min weight fraction and par2 with max weight fraction
  cpdat_pi_uniform_sub<- cpdat_pi[!is.na(cpdat_pi$clean_min_wf) & !is.na(cpdat_pi$clean_max_wf),]
  cpdat_pi_uniform <- cpdat_pi_uniform_sub %>%
    mutate(dist.type = rep(paste("uniform"),length(nrow))) %>%
    select(source.type,
           pucid,
           form,
           dtxsid,
           min.age,
           max.age,
           gender,
           pr.chem,
           form,
           dist.type,
           par1 = clean_min_wf,
           par2 = clean_max_wf,
           document_id, #Added temporarily to track down weird puc/form matches. Remove before final product!
           product_id, #Added temporarily to track down weird puc/form matches. Remove before final product!
           chemical_id, #Added temporarily to match to original factotum download
           prod_title,
           prod_type) 
##Do a quick check of NA's to make sure there aren't any values in central, as those would be point and not uniform.
##Also check to make sure there aren't any NAs in min or max, as we need all of those values present for uniform
sum(is.na(cpdat_pi_uniform_sub$clean_min_wf))        #Check no NAs
sum(is.na(cpdat_pi_uniform_sub$clean_max_wf))        #Check no NAs
sum(!is.na(cpdat_pi_uniform_sub$clean_central_wf))   #Check all NAs
  
##If clean weight data is NOT available for min and max but is for central, use point distribution for dist.type and fill par1 with central weight fraction 
  cpdat_pi_point_sub<- cpdat_pi[is.na(cpdat_pi$clean_min_wf) & is.na(cpdat_pi$clean_max_wf) & !is.na(cpdat_pi$clean_central_wf),]
  cpdat_pi_point<- cpdat_pi_point_sub %>%
    mutate(dist.type = rep(paste("point"),length(nrow)),
           par2 = rep("", length(nrow))) %>%
    select(source.type,
           pucid,
           form,
           dtxsid,
           min.age,
           max.age,
           gender,
           pr.chem,
           form,
           dist.type,
           par1 = clean_central_wf,
           par2,
           document_id,
           product_id,
           chemical_id,
           prod_title,
           prod_type)

##Do a quick check of NA's to make sure there aren't any values in min and max, as those would be uniform and not point.
##Also check to make sure there aren't any NAs in central, as we need all of those values present 
sum(!is.na(cpdat_pi_point_sub$clean_min_wf))        #Check all NAs
sum(!is.na(cpdat_pi_point_sub$clean_max_wf))        #Check all NAs
sum(is.na(cpdat_pi_point_sub$clean_central_wf))   #Check NO NAs

##---------------------------------------STEP 4: Combine  uniform and point dataframes to create a clean source_chem_file ---------- 
cpdat_pi_both <- rbind(cpdat_pi_uniform, cpdat_pi_point)
cpdat_pi_new <-cpdat_pi_both %>% mutate(par3 = rep("", length(nrow)),
                                  par4 = rep("", length(nrow)),
                                  l.trunc = rep("", length(nrow)),
                                  u.truc = rep("", length(nrow)),
                                  resamp = rep("", length(nrow))) %>% 
  arrange(pucid, dtxsid) %>%
  filter(!is.na(pucid), !is.na(dtxsid))

str(cpdat_pi_new)

##--------------------------------------STEP 5: Drop unwanted forms and finish cleaning-------------------------------------

cpdat_pi_cleanform <- cpdat_pi_new %>% 
  group_by(pucid, form, dtxsid) %>%
  mutate(product.id = seq(1:n()),
         form = str_trim(form))   %>%
  mutate(form = str_replace(form, "\\|", " ")) %>%
  mutate(form = str_replace(form, "pump$", "pump spray")) %>%
  filter(form != "child", form != "exterior", form != "interior", form != "in vehicle", form != "granules")

stop()

##Export list of new dtxsids for Kristin 
#old.chems <- unique(source_chem_original$Chemical)
#write.csv(old.chems, "old_chems.csv", row.names = F)
#old.batchsearch <- read.csv("C:/Users/vhull/OneDrive - Environmental Protection Agency (EPA)/Profile/Downloads/CompToxChemicalsDashboard-Batch-Search_2021-08-26_15_23_04.csv")
#old.castodtxsid <- unique(old.batchsearch$DTXSID)
#old.dtxsid <- unique(source_chem_example$dtxsid)
#new.chems <- unique(cpdat_pi_cleanform$dtxsid)

#chems.needopera <- new.chems[which(!(new.chems %in% old.castodtxsid))]
#write.csv(chems.needopera, "chemforshedsopera_08262021.csv", row.names = F)

##--------------------------------------STEP 6: Check for and remove duplicates---------------------------------------------
dup_check <- cpdat_pi_cleanform %>%
  arrange(prod_title, dtxsid, product_id)
dup_check$prod_title <- tolower(dup_check$prod_title)
dup_check$prod_title <- trimws(dup_check$prod_title)

dupprods_step1 <- dup_check %>%
  mutate(indicator = paste0(prod_title, " ", dtxsid, " ", par1, " ", par2))

##FIX THE THE ORDER SO ITS IDENTICAL 

prod_titles <- unique(dupprods_step1$prod_title)
product_number <- unique(dupprods_step1$product_id)
mega.dup.dataframe <- NULL
for(i in 1:length(product_number)){
    dupprods_sub2 <- dupprods_sub1[dupprods_sub1$product_id == product_number[i],]
    vectorindicator <- NULL
    for(k in 1:nrow(dupprods_sub2)){
      vectorindicator[k] <- dupprods_sub2$indicator[k] 
    }
    if(!is.na(vectorindicator)){
      largeindicator <- toString(vectorindicator)
      indprod <- cbind(largeindicator, product_number[j])
      mega.dup.dataframe <- rbind(mega.dup.dataframe, indprod)
    }
  }
mega.dup.dataframe <- as.data.frame(mega.dup.dataframe2)



##Only keep distinct "largeindicator" product numbers
final.duplicated <- mega.dup.dataframe2 %>%
  distinct(largeindicator, .keep_all = TRUE)

##And finally, only keep product numbers  "large indicator products"

cpdat_pi_nodup <- cpdat_pi_cleanform %>%
  filter(product_id %in% final.duplicated$V2)


##--------------------------------------STEP 7: Create a product.id (replacing the previous form.id) -------------------------
#Create product.id
cpdat_pi_id<- cpdat_pi_nodup%>%
  relocate(product.id, .after = dtxsid) %>%
  group_by(pucid, form, dtxsid) %>%
  mutate(product.id = seq(1:n())) 
  
#write.csv(cpdat_pi_id, "source_chem_cpdat_08262021.csv", row.names = F)


##=========================================================== OLD CODE, SAVED FOR POSTERITY BUT NOT USED ==============================================

#The new cpdat file needs to be compared to the source_scen file. If any product/form pairs are missing from the source_scen file they must be added or SHEDS will not work.
##source.scen
source_scen_2 <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/SHEDS Project/SHEDSDevel-ICF-DEVEL/Inputs/source_scen_2.csv", header = T)
str(source_scen_2)

cpdat_id_indicator <- cpdat_pi_id %>%
  mutate(indicator = paste0(pucid, " ", form)) %>%
  filter(indicator != "NA NA")

source_scen_indicator <- source_scen_2 %>%
  mutate(indicator = paste0(pucid, " ", form))

add_form <- cpdat_id_indicator[which(cpdat_id_indicator$indicator %in% source_scen_indicator$indicator == FALSE),]
unique_forms <- unique(add_form$indicator)

cat(paste0("I need to add ", length(unique_forms), " new puc_form pairs to the source scen file"))





##===========================================================================
#Are there any single ones 
cpdat_pi_sub<- cpdat_pi_new %>%
  mutate(form = str_replace(form, "\\|", " ")) %>%
  mutate(form = str_replace(form, "pump$", "pump spray")) %>%
  filter(form == "child" | form == "exterior"| form == "interior"| form == "in vehicle") %>%
  mutate(indicator = paste0(pucid, " ", form)) %>%
  filter(indicator != "NA NA")

l_ind
 
paste0("indicator ", "HM.1400.012 exterior" ," from document_id ", unique(cpdat_id_indicator$document_id[cpdat_id_indicator$indicator == "HM.1400.012 exterior" ]),
           " and product_id ", unique(cpdat_id_indicator$product_id[cpdat_id_indicator$indicator == "HM.1400.012 exterior" ]))


cpdat_id_indicator[cpdat_id_indicator$indicator == "AC.0500.040 child",]




##To make new source_scen from scratch 
source_scen_temp <- chem_data %>%
  left_join(weight_data, by = c("chemical_id"="chemical_id"))%>%
  left_join(puc_data, by = c("puc_id" = "puc_id")) %>%
  left_join(form_dat, by = c("puc_code" ="PUCID")) %>%
  mutate(source.type = rep(paste("product"), length(nrow)),
         form = ifelse(is.na(attribute_name), Default.Form, attribute_name)) %>%
  group_by(puc_code, form) %>%
  select(source.type,
         pucid = puc_code,
         product = prod_type,
         form) %>%
  distinct()%>%
  drop_na() %>%
  arrange(pucid, form)
source_scen_new <- source_scen_temp %>%
  group_by(pucid) %>%
  mutate(freq = n(),     
         prevalence = 1/freq)
write.csv(source_scen_new, "source_scen_2-24-2021-1000.csv")


