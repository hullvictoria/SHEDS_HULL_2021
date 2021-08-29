##AUTHOR: Victoria Hull, ORAU scc
##Last edited 1/29/2021 - Changed SHEDS ID to being a code for each personday,
##fixed day1 and day2 issue in crops


#Workspace prep

#Haven is used to read in xpt, dplyr is used for data formatting and reorganization
library(haven); library(dplyr)
#Set working directory to where you want to store your outputs.  
setwd("C:/Users/Administrator/OneDrive/Profile/Documents/WWEIA/WWEIA Files")

#==================================================================================
#Step 1: Read in files from NHANES. 
#Naming conventions change after 2002, so you will need two different scripts for this

#FUNCTION TO READ FILES FOR 2003-2018
read.write.files <- function(x, y, z){
  day1<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x ,"/DR1IFF_", y, ".XPT"))
  write.csv(day1, paste0("day1_",z,".csv"), row.names = F)
  
  day2<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x , "/DR2IFF_", y, ".XPT"))
  write.csv(day2, paste0("day2_",z,".csv"), row.names = F)
  
  foodcodes<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x ,"/DRXFCD_", y, ".XPT"))
  write.csv(foodcodes, paste0("foodcodes_",z,".csv"), row.names = F)
  
  demo<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x , "/DEMO_", y, ".XPT"))
  write.csv(demo, paste0("demo_",z,".csv"), row.names = F)
  
  body<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x , "/BMX_", y, ".XPT"))
  write.csv(body, paste0("body_",z,".csv"), row.names = F)
}

#vector with naming conventions
data.label.0318 <-as.data.frame(cbind(c("2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"), 
                                      c("C","D","E","F","G","H","I","J"), c("03_04", "05_06", "07_08", "09_10","11_12","13_14","15_16", "17_18")))
for(i in 1:length(data.label.0318)) {
  x <- data.label.0318[i,1]
  y <- data.label.0318[i,2]
  z <- data.label.0318[i,3]
  read.write.files(x,y,z)
}


#FUNCTION TO READ FILES FOR 1999-2002
read.write.files.9902 <- function(x, y, z){
  #day <- read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x ,"/DRXIFF", y, ".XPT"))  #Accessibility issue for these files. It's only two files, I recommend reading them in manually
  #write.csv("day_",z,".csv")
  
  foodcodes<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x ,"/DRXFMT", y, ".XPT"))
  write.csv(foodcodes, paste0("foodcodes_",z,".csv"), row.names = F)
  
  demo<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x , "/DEMO", y, ".XPT"))
  write.csv(demo, paste0("demo_",z,".csv"), row.names = F)
  
  body<-read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", x , "/BMX", y, ".XPT"))
  write.csv(body, paste0("body_",z,".csv"), row.names = F)
}

data.label.9902 <- as.data.frame(cbind(c("1999-2000","2001-2002"), c("","_B"), c("99_00","01_02")))

for(i in 1:2) {
  x <- data.label.9902[i,1]
  y <- data.label.9902[i,2]
  z <- data.label.9902[i,3]
  read.write.files.9902(x,y,z)
}

##===============================================================================
#STEP 2: Clean NHANES data
#For our model, I need a demographic file with body weight, gender and age and a diet diary file with food codes, modification codes, and food weights

#CREATE DEMOGRAPHIC AND NHANES WWEIA FOOD CODE INPUT FILES FOR PHASES 2003-2018

create_phase_tables_0318<-function(year){
  
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  day2<-read.csv(paste0("day2_",year,".csv"), header = T)
  foodcodes<-read.csv(paste0("foodcodes_",year,".csv"), header = T)
  demog<-read.csv(paste0("demo_",year,".csv"), header = T)
  bodyw<-read.csv(paste0("body_",year,".csv"), header = T)
  
  #Create Demographic table with age, gender, and bodyweight
  tbl_dietdemo <- demog %>%
    left_join(bodyw, by = c("SEQN" = "SEQN")) %>%
    mutate(RIAGENDR=recode(RIAGENDR,                #Turn gender into M and F rather than 1 and 2 
                           `1`="M",
                           `2`="F"),
           phase = year) %>%    
    select(SEQN= SEQN,
           phase,
           gender = RIAGENDR,
           age = RIDAGEYR,
           weight = BMXWT)
  write.csv(tbl_dietdemo, paste0("diet_demo_",year,".csv"), row.names = F)
  
  #Create diet diary table for with NHANES food codes - day1 table
  tbl1_diet_foodcode <- day1 %>%
    left_join(foodcodes, by=c("DR1IFDCD" ="DRXFDCD")) %>%
    mutate(phase = year,
           day = as.numeric(rep(1, length(nrow)))) %>%  #I added in a column for day 
    select(SEQN,
           phase,
           day,
           food_codes = DR1IFDCD,
           food_weight = DR1IGRMS,
           code_description = DRXFCSD)
  #Create diet diary table for with NHANES food codes - day2 table
  tbl2_diet_foodcode<- day2 %>%
    left_join(foodcodes, by=c("DR2IFDCD" ="DRXFDCD")) %>%
    mutate(phase = year,
           day = as.numeric(rep(1, length(nrow)))) %>%
    select(SEQN,
           phase,
           day,
           food_codes = DR2IFDCD,
           food_weight = DR2IGRMS,
           code_description = DRXFCSD)
  codedietdiary<-rbind(tbl1_diet_foodcode,tbl2_diet_foodcode)
  write.csv(codedietdiary, paste0("diet_diary_",year,".csv"), row.names = F)
}

sapply(data.label.0318[,3], create_phase_tables_0318)


#CREATE INPUT FILES FOR PHASES 1999-2001
#There is only one day for these phases, so only need one day
create_phase_tables_9902<-function(year){
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  foodcodes<-read.csv(paste0("foodcodes_",year,".csv"), header = T)
  demog<-read.csv(paste0("demo_",year,".csv"), header = T)
  bodyw<-read.csv(paste0("body_",year,".csv"), header = T)
  #Create Demographic table with age, gender, and bodyweight
  tbl_dietdemo <- demog %>%
    left_join(bodyw, by = c("SEQN" = "SEQN")) %>%
    mutate(RIAGENDR=recode(RIAGENDR, 
                           `1`="M",
                           `2`="F"),
           phase = year) %>%
    select(SEQN,
           phase = phase,
           gender = RIAGENDR,
           age = RIDAGEYR,
           weight = BMXWT)
  write.csv(tbl_dietdemo, paste0("diet_demo_",year,".csv"), row.names = F)
  #Create diet diary table for with NHANES food codes - day1 table
  tbl1_diet_foodcode <- day1 %>%
    left_join(foodcodes, by=c("DRDIFDCD" ="START")) %>%
    mutate(phase = year,
           day = as.numeric(rep(1, length(nrow)))) %>%
    select(SEQN,
           day = day,
           phase = phase,
           food_codes = DRDIFDCD,
           food_weight = DRXIGRMS,
           code_description = LABEL)
  write.csv(tbl1_diet_foodcode, paste0("diet_diary_",year,".csv"), row.names = F)
}

sapply(data.label.9902[,3], create_phase_tables_9902)


##COMBINE INPUT FILES - change this based on the files you're interested in. We're focusing on these years
diet_demo_11_12 <- read.csv("diet_demo_11_12.csv")
diet_demo_09_10 <- read.csv("diet_demo_09_10.csv")
diet_demo_07_08 <- read.csv("diet_demo_07_08.csv")
diet_demo_05_06 <- read.csv("diet_demo_05_06.csv")
diet_demo <- rbind(diet_demo_11_12, diet_demo_09_10, diet_demo_07_08, diet_demo_05_06)
write.csv(diet_demo, "diet_demo.csv", row.names = F)


diet_diary_11_12 <- read.csv("diet_diary_11_12.csv")
diet_diary_09_10 <- read.csv("diet_diary_09_10.csv")
diet_diary_07_08 <- read.csv("diet_diary_07_08.csv")
diet_diary_05_06 <- read.csv("diet_diary_05_06.csv")
diet_diary <- rbind(diet_diary_11_12, diet_diary_09_10, diet_diary_07_08, diet_diary_05_06)
write.csv(diet_diary, "diet_code_diary.csv", row.names = F)


##=====================================================================================================================
#CREATE COMMODITY GROUPS
#This is based on both mod codes and food codes in 1999-2012. They stopped using mod codes in 2013 and instead added more food codes to account for the modification
#This means that the recipe files will not match the 
#For 2013-2018 filtered out mod code recipes

#Download these manually from the fcid website and store them in your downloads folder 

fcid_recipe <- read.csv("Recipes_WWEIA_FCID_0510.csv", header = T) #Translates food codes to fcid commodity groups
fcid_descrip <- read.csv("FCID_Code_Description.csv" , header = T) #Gives descriptions for fcid commodity groups

#2013-2018 Commodity diet files
create_commodity_tables_1318<-function(year){
  
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  day2<-read.csv(paste0("day2_",year,".csv"), header = T)
  #Translate food codes to commodity groups - day 1
  tbl_fcid1 <- day1%>%
    left_join(fcid_recipe, by = c("DR1IFDCD" = "Food_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    mutate(commodity_weight_grm = (Commodity_Weight/100)*DR1IGRMS) %>%
    filter(Mod_Code == 0) %>%
    select(SEQN,
           food_code = DR1IFDCD,
           food_weight = DR1IGRMS,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm) %>%
    arrange(respondent_id, commodity_group)
  #There are more food codes than commodity groups. We want to combine commodity groups for each person on each day
  #Note: you don't actually need to do this as a seperate step. I did it this way to help me troubleshoot
  tbl_fcidsum1<- tbl_fcid1 %>%
    group_by(commodity_group, respondent_id) %>%
    mutate(commodity_weight_sum = sum((commodity_weight_grm)), 
           phase = year,
           day = as.numeric(rep(1, length(nrow)))) %>%
    select(SEQN,
           phase = phase,
           day = day,
           commodity_group,
           commodity_description,
           commodity_weight = commodity_weight_sum) %>%
    arrange(respondent_id, commodity_group) %>%
    distinct(commodity_group, .keep_all = TRUE)
  #Translate food codes to commodity groups - day 1
  tbl_fcid2 <- day2%>%
    left_join(fcid_recipe, by = c("DR2IFDCD" = "Food_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR2IGRMS) %>%
    filter(Mod_Code == 0) %>%    #In 2013 they replaced modification codes with more unique food groups. We are not interested in mod codes, so filter them out                                   
    select(SEQN,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm) %>%
    arrange(respondent_id, commodity_group)
  
  tbl_fcidsum2<- tbl_fcid2 %>%
    group_by(commodity_group, respondent_id) %>%
    mutate(commodity_weight_sum = sum((commodity_weight_grm)),
           phase = year,
           day = as.numeric(rep(2, length(nrow))))%>%
    select(SEQN,
           phase = phase,
           day = day,
           commodity_group,
           commodity_description,
           commodity_weight = commodity_weight_sum) %>%
    arrange(respondent_id, commodity_group) %>%
    distinct(commodity_group, .keep_all = TRUE)
  #combine day1 and day 2, write csv
  tbl_fcid_both <- rbind(tbl_fcidsum1, tbl_fcidsum2)
  write.csv(tbl_fcid_both, paste0("commodity_diary_",year,".csv"), row.names = F)}

sapply(c("13_14","15_16","17_18"), create_commodity_tables_1318)

#Commodity tables for 03-12. Code incorporates modification codes
create_commodity_tables_0312<-function(year){
  
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  day2<-read.csv(paste0("day2_",year,".csv"), header = T)
  
  tbl_fcid1 <- day1%>%
    left_join(fcid_recipe, by = c("DR1IFDCD" = "Food_Code", "DR1MC" = "Mod_Code")) %>%  #Join on both food codes AND modification codes to get correct weight
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    mutate(commodity_weight_grm = (Commodity_Weight/100)*DR1IGRMS) %>%
    select(SEQN = SEQN,
           food_code = DR1IFDCD,
           mod_code = DR1MC,
           food_weight = DR1IGRMS,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidsum1<- tbl_fcid1 %>%
    group_by(commodity_group, SEQN) %>%
    mutate(commodity_weight_sum = sum(commodity_weight_grm),
           day = as.numeric(rep(1, length(nrow))),
           phase = paste0(year))%>%
    select(SEQN,
           day = day,
           phase = phase,
           commodity_group,
           commodity_description,
           commodity_weight = commodity_weight_sum) %>%
    arrange(SEQN, commodity_group) %>%
    distinct(commodity_group, .keep_all = TRUE)  
  
  
  tbl_fcid2 <- day2%>%
    left_join(fcid_recipe, by = c("DR2IFDCD" = "Food_Code", "DR2MC" = "Mod_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR2IGRMS) %>%
    select(SEQN,
           mod_coday= DR2MC,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidsum2<- tbl_fcid2 %>%
    group_by(commodity_group, SEQN) %>%
    mutate(commodity_weight_sum = sum(commodity_weight_grm),
           day = as.numeric(rep(2, length(nrow))),
           phase = paste0(year)) %>%
    select(SEQN,
           day = day,
           phase = phase,
           commodity_group,
           commodity_description,
           commodity_weight = commodity_weight_sum) %>%
    arrange(SEQN, commodity_group) %>%
    distinct(commodity_group, .keep_all = TRUE)
  
  tbl_fcid_both <- rbind(tbl_fcidsum1, tbl_fcidsum2)
  write.csv(tbl_fcid_both, paste0("commodity_diary_",year,".csv"), row.names = F)}

sapply(c("03_04", "05_06","07_08","09_10","11_12"), create_commodity_tables_0312)


#Commodity codes for 99-02. Have not yet figured out how to incorporate modification codes 

create_commodity_tables_9902<-function(year){
  
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  
  tbl_fcid1 <- day1%>%
    left_join(fcid_recipe, by = c("DRDIFDCD" = "Food_Code"), c("DRDMRUF" = "Mod_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DRXIGRMS) %>%
    select(SEQN,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidsum1<- tbl_fcid1 %>%
    group_by(commodity_group, SEQN) %>%
    mutate(commodity_weight_sum = sum(commodity_weight_grm),
           phase = year,
           day = as.numeric(rep(1, length(nrow)))) %>%
    select(SEQN,
           phase = phase,
           day = day,
           commodity_group,
           commodity_description,
           commodity_weight = commodity_weight_sum) %>%
    arrange(SEQN, commodity_group) %>%
    distinct(commodity_group, .keep_all = TRUE)
  
  write.csv(tbl_fcidsum1, paste0("commodity_diary_",year,".csv"), row.names = F)}

sapply(c("99_00","01_02"), create_commodity_tables_9902)

##Combine to make one large file
commodity_diary_17_18 <- read.csv("commodity_diary_17_18.csv")
commodity_diary_15_16 <- read.csv("commodity_diary_15_16.csv")
commodity_diary_13_14 <- read.csv("commodity_diary_13_14.csv")
commodity_diary_11_12 <- read.csv("commodity_diary_11_12.csv")
commodity_diary_09_10 <- read.csv("commodity_diary_09_10.csv")
commodity_diary_07_08 <- read.csv("commodity_diary_07_08.csv")
commodity_diary_05_06 <- read.csv("commodity_diary_05_06.csv")
commodity_diary_03_04 <- read.csv("commodity_diary_03_04.csv")
commodity_diary_01_02 <- read.csv("commodity_diary_01_02.csv")
commodity_diary_99_00 <- read.csv("commodity_diary_99_00.csv")

commodity_diary <- rbind(commodity_diary_17_18, commodity_diary_15_16, commodity_diary_13_14, commodity_diary_11_12, commodity_diary_09_10,
                         commodity_diary_07_08, commodity_diary_05_06, commodity_diary_03_04, commodity_diary_01_02, commodity_diary_99_00)
write.csv(commodity_diary, "commodity_code_diary.csv", row.names = F)



#COMBINE TO MAKE A COMMODITY TABLE FROM 05-12
commodity_diary_11_12 <- read.csv("commodity_diary_11_12.csv")
commodity_diary_09_10 <- read.csv("commodity_diary_09_10.csv")
commodity_diary_07_08 <- read.csv("commodity_diary_07_08.csv")
commodity_diary_05_06 <- read.csv("commodity_diary_05_06.csv")
commodity_diary <- rbind(commodity_diary_11_12, commodity_diary_09_10, commodity_diary_07_08, commodity_diary_05_06)


commodity_diary %>% 
  mutate(SHEDS_ID = 1:nrow(commodity_diary))

#fix
new_commodity <- commodity_diary %>% 
  mutate(SHEDS_ID = group_indices(., SEQN, day)) %>% arrange(desc(SEQN), day)

write.csv(new_commodity_diary, "commodity_code_diary.csv", row.names = F)


#Source_scen_file for commodity codes
commodity_code_diary <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/WWEIA/WWEIA Files/commodity_code_diary.csv", header = T)
source_scen_commodity <- commodity_code_diary %>%
  mutate(source.type = rep("Food", length(nrow)),
         form = rep("form1", length(nrow)),
         indoor = rep("1", length(nrow)),
         dietary = rep("1", length(nrow)),
         migration = rep("0", length(nrow)),
         dirderm = rep("0", length(nrow)),
         diringest = rep("0", length(nrow)),
         dirinhvap = rep("0", length(nrow)),
         downthedrain = rep("0", length(nrow)),
         indir.fug = rep("0", length(nrow)),
         indir.y0 = rep("0", length(nrow))) %>%
  select(source.type,
         pucid = commodity_group,
         form,
         description = commodity_description,
         indoor,
         dietary,
         migration,
         dirderm,
         diringest,
         dirinhvap,
         downthedrain,
         indir.fug,
         indir.y0) %>%
  distinct_all()
write.csv(source_scen_commodity, "source_scen_commodity.csv")

#FOOD CROP CONNECTIONS
##Same thing as commodity groups. I need to match on modification code as well as food code for 1999-2012. For 2013-2018
fcid_crop <- read.csv("FCID_Cropgroup_Description.csv", header = T)


#Function to create crop tables for 2013-2018.
create_crop_tables_1318 <- function (year){
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  day2<-read.csv(paste0("day2_",year,".csv"), header = T)
  
  tbl_fcidcrop <- day1%>%
    left_join(fcid_recipe, by = c("DR1IFDCD" = "Food_Code")) %>%        
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    left_join(fcid_crop, by = c("CG_Subgroup" = "CGL")) %>%
    filter(Mod_Code == 0) %>%                                            #No mod codes for these phases, so exclude files with modcodes 
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR1IGRMS) %>%  #Translate weight % in fcid recipes to weight in grams
    select(SEQN,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm,
           crop_group = CG_Subgroup,
           crop_description = Crop_Group_Description) %>%
    arrange(SEQN, commodity_group)
  

  tbl_fcidcropsum<- tbl_fcidcrop %>%                                    
    group_by(crop_group, SEQN) %>%
    mutate(crop_weight_sum = sum(commodity_weight_grm),
           phase = year,
           day = as.numeric(rep(1, length(nrow))))%>% 
    select(SEQN,
           phase,
           day,
           crop_group,
           crop_description,
           crop_weight = crop_weight_sum) %>%
    arrange(SEQN, crop_group) %>%
    distinct(crop_group, .keep_all = TRUE)
  
  tbl_fcidcrop2 <- day2%>%
    left_join(fcid_recipe, by = c("DR2IFDCD" = "Food_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    left_join(fcid_crop, by = c("CG_Subgroup" = "CGL")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR2IGRMS) %>%
    filter(Mod_Code == 0) %>%
    select(SEQN,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm,
           crop_group = CG_Subgroup,
           crop_description = Crop_Group_Description) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidcropsum2<- tbl_fcidcrop2 %>%
    group_by(crop_group, SEQN) %>%
    mutate(crop_weight_sum = sum(unique(commodity_weight_grm)),
           phase = year,
           day = as.numeric(rep(2, length(nrow))))%>%
    select(SEQN,
           phase,
           day,
           crop_group,
           crop_description,
           crop_weight = crop_weight_sum) %>%
    arrange(SEQN, crop_group) %>%
    distinct(crop_group, .keep_all = TRUE)
  
  tbl_crops <- rbind(tbl_fcidcropsum, tbl_fcidcropsum2)
  write.csv(tbl_crops, paste0("crop_diary_",year,".csv"), row.names = F)}

sapply(c("13_14","15_16","17_18"), create_crop_tables_1318)


#Function to create crop tables for 2003-2012.
create_crop_tables_0312 <- function (year){
  day1<-read.csv(paste0("day1_",year,".csv"), header = T)
  day2<-read.csv(paste0("day2_",year,".csv"), header = T)
  
  tbl_fcidcrop <- day1%>%
    left_join(fcid_recipe, by = c("DR1IFDCD" = "Food_Code","DR1MC" = "Mod_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    left_join(fcid_crop, by = c("CG_Subgroup" = "CGL")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR1IGRMS) %>%
    select(SEQN,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm,
           crop_group = CG_Subgroup,
           crop_description = Crop_Group_Description) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidcropsum<- tbl_fcidcrop %>%
    group_by(crop_group, SEQN) %>%
    mutate(crop_weight_sum = sum(commodity_weight_grm),
           phase = year,
           day = as.numeric(rep(1, length(nrow))))%>%
    select(SEQN,
           phase, 
           day,
           crop_group,
           crop_description,
           crop_weight = crop_weight_sum) %>%
    arrange(SEQN, crop_group) %>%
    distinct(crop_group, .keep_all = TRUE)
  
  tbl_fcidcrop2 <- day2%>%
    left_join(fcid_recipe, by = c("DR2IFDCD" = "Food_Code","DR2MC" = "Mod_Code")) %>%
    left_join(fcid_descrip, by = c("FCID_Code" = "FCID_Code")) %>%
    left_join(fcid_crop, by = c("CG_Subgroup" = "CGL")) %>%
    mutate(commodity_weight_grm =  (Commodity_Weight/100)*DR2IGRMS) %>%
    select(SEQN,
           mod_code = DR2MC,
           commodity_group = FCID_Code,
           commodity_description = FCID_Desc,
           commodity_weight_percent = Commodity_Weight,
           commodity_weight_grm,
           crop_group = CG_Subgroup,
           crop_description = Crop_Group_Description) %>%
    arrange(SEQN, commodity_group)
  
  tbl_fcidcropsum2<- tbl_fcidcrop2 %>%
    group_by(crop_group, SEQN) %>%
    mutate(crop_weight_sum = sum(unique(commodity_weight_grm)),
           phase = year,
           day = as.numeric(rep(2, length(nrow))))%>%
    select(SEQN,
           phase,
           day,
           crop_group,
           crop_description,
           crop_weight = crop_weight_sum) %>%
    arrange(SEQN, crop_group) %>%
    distinct(crop_group, .keep_all = TRUE)
  
  tbl_crops <- rbind(tbl_fcidcropsum, tbl_fcidcropsum2)
  write.csv(tbl_crops, paste0("crop_diary_",year,".csv"), row.names = F)}

sapply(c("03_04", "05_06","07_08","09_10","11_12"), create_crop_tables_0312)


#COMBINE TO MAKE A COMMODITY TABLE FROM 05-12
crop_diary_11_12 <- read.csv("crop_diary_11_12.csv")
crop_diary_09_10 <- read.csv("crop_diary_09_10.csv")
crop_diary_07_08 <- read.csv("crop_diary_07_08.csv")
crop_diary_05_06 <- read.csv("crop_diary_05_06.csv")
crop_diary <- rbind(crop_diary_11_12, crop_diary_09_10, crop_diary_07_08, crop_diary_05_06)

##FIX - WHERE IS DAY 2??
new <- crop_diary %>% 
  mutate(SHEDS_ID = group_indices(., SEQN, day))

write.csv(new, "crop_code_diary.csv", row.names = F)

crop_diary %>% 
  mutate(SHEDS_ID = 1:nrow(crop_diary))

write.csv(crop_diary, "crop_code_diary.csv", row.names = F)

##Source_scen files
crop_code_diary <- read.csv("C:/Users/Administrator/OneDrive/Profile/Documents/WWEIA/WWEIA Files/crop_code_diary.csv", header = T)
source_scen_crop <- crop_code_diary %>%
  mutate(source.type = rep("Food", length(nrow)),
         form = rep("form1", length(nrow)),
         indoor = rep("1", length(nrow)),
         dietary = rep("1", length(nrow)),
         migration = rep("0", length(nrow)),
         dirderm = rep("0", length(nrow)),
         diringest = rep("0", length(nrow)),
         dirinhvap = rep("0", length(nrow)),
         downthedrain = rep("0", length(nrow)),
         indir.fug = rep("0", length(nrow)),
         indir.y0 = rep("0", length(nrow))) %>%
  select(source.type,
         pucid = crop_group,
         form,
         description = crop_description,
         indoor,
         dietary,
         migration,
         dirderm,
         diringest,
         dirinhvap,
         downthedrain,
         indir.fug,
         indir.y0) %>%
  distinct_all()
write.csv(source_scen_crop, "source_scen_crop.csv")


#Fake source_chem until I figure out the true weight fraction data
#This is for atrazine in cereal grains and is based on source_chem_4

source_chem_crops <- data.frame()
source_chem_crops <- source_chem_crops %>%
  mutate(source.type = c("Article", "Product", "Food", "Food"),
         pucid = c("A.001", "P.001", "15", "15"),
         puc.description = c("Article 001", "Product 001", "Food; 15 Cereal Grains","Food; 15 Cereal Grains"),
         formulation = rep(form1, length(nrow)),
         product.id = rep("1", length(nrow)),
         variable = c("y0", "f.chemical", "migration", "residue"),
         units = c("[-]", "[-]", "ug/g", "ug/g"),
         prevalence
         )

