### Association between deprivation and the prescribing of oral antibacterials in primary care ###
### Dahiyat S et al. 2024  ###


### Define paths

rm(list=ls()) # Clear environment

path.project <- "C:/Users/sirsa/OneDrive/Documents/2024Dahiyat"
path.data.prescribing <- "C:/Users/sirsa/OneDrive/Desktop/Warwick/Research/Data/EPD"


### Load packages -----

#### CONTINUE FROM HERE ####

list.of.packages <- c("data.table", "dplyr","fingertipsR","jsonlite", "crul","janitor", "readxl", "Cairo", "ggplot2", "sandwich", "msm", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(data.table)
library(fingertipsR)
library(jsonlite)
library(crul)
library(janitor)
library(readxl)
library(Cairo)
library(ggplot2)
library(sandwich)
library(msm)
library(stringr)

### Load data -----

## Load and clean prescribing data

# Select BNF codes for analysis
setwd(path.project) # Select project folder
keep <- read.csv("bnf_codes.csv") # Read in BNF codes, from https://applications.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126
keep <- as.vector(keep$bnf_code) # List as vector

# Set working directory
setwd(path.data.prescribing)

# Define data months to include
included.months <- list("202204", "202205", "202206", "202207", "202208", "202209", "202210", "202211", "202212", "202301", "202302", "202303")

# Read in data
for(i in included.months){
  file = paste0("EPD_",i,".csv") # define file naming convention
  data <- fread(file = file, header = T, sep = ",") # read in data file
  data1 <- data[data$`BNF_CODE` %in% keep, ] # keep only entries with a BNF code of relevance
  data2 <- data1 %>% dplyr::select(YEAR_MONTH,PRACTICE_CODE,BNF_CHEMICAL_SUBSTANCE,CHEMICAL_SUBSTANCE_BNF_DESCR,BNF_CODE,BNF_DESCRIPTION,QUANTITY,ITEMS,TOTAL_QUANTITY,NIC,ACTUAL_COST)
  assign(paste0("data",i), data2) # save as dataframe object
  rm(data, data1, data2)
}

# Combine prescribing data
dfs <- lapply(ls(pattern="^data"), function(x) get(x)) # list all epd monthly dataframes
data_epd <- rbindlist(dfs) # bind all epd dataframes
rm(list = ls(pattern="^data2"), dfs) # remove unused dataframes

# Truncate BNF codes to only include the chemical code (first 9 characters)
data_epd$BNF_CODE <- str_sub(data_epd$BNF_CODE,1,9)

# Aggregate data by BNF code and practice code
data_epd <- setDT(data_epd)[,.(ITEMS = sum(ITEMS),TOTAL_QUANTITY = sum(TOTAL_QUANTITY),NIC = sum(NIC),`ACTUAL_COST`=sum(`ACTUAL_COST`)), by = .(PRACTICE_CODE,`BNF_CODE`)] %>%
  clean_names()


## Load IMD scores by practice (1 least deprived, 5 most deprived)

# Fingertips API (see https://fingertips.phe.org.uk/documents/fingertips_api_guide.pdf and https://github.com/ropensci/fingertipsR)

# profiles <- profiles() # Show PHOF profiles. Use to identify which profile set you need. Here we use profile ID 21 [National General Practice Profiles (supporting indicators)]

# inds <- indicators(ProfileID = "21") # Show indicators for selected PHOF profile. Here we use indicator ID 93553 [IMD 2019 score]

# areas <- area_types() # Show area types. Here we use area type 7 [General practice]

data_imd <- fingertips_data(IndicatorID = 93553, AreaTypeID = 7) %>% # IMD scores by GP practice
  # Keep IMD(2019) scores only
  filter(Timeperiod == "2019") %>% 
  # Remove England value
  filter(AreaType != "England") %>% 
  # Keep required fields only
  select(AreaCode, AreaName, ParentName, Value) %>% 
  # Rename columns
  dplyr::rename("practice_code" = "AreaCode", 
                "gp.name" = "AreaName",
                "pcn.name" = "ParentName",
                "imd.score" = "Value") %>%
  # Keep required columns
  select(practice_code, imd.score)

## Load practice list size [From catalyst.services.nhsbsa.nhs.uk/analytics]

# Load list size download
setwd(path.project) # select project folder

data_listsize <- read_xlsx("Patient population and list size.xlsx", skip = 8, sheet = 1) %>% # read in data
  clean_names()

# Select required listsize data, including number of females

data_listsize <- data_listsize %>%
  # select required columns
  select(code_8,
         total_list_size,
         female_00_04,
         female_05_14,
         female_15_24,
         female_25_34,
         female_35_44,
         female_45_54,
         female_55_64,
         female_65_74,
         female_75,
         male_55_64,
         male_65_74,
         male_75) %>%
  # calculate total females
  mutate(total.females = female_00_04 + female_05_14 + female_15_24 + female_25_34 + female_35_44 + female_45_54 + female_55_64 + female_65_74 + female_75) %>%
  # calculate total >55yr
  mutate(total.older = female_55_64 + female_65_74 + female_75 + male_55_64 + male_65_74 + male_75) %>%
  # select required columns
  select(code_8, total.females, total.older, total_list_size) %>%
  # rename practice code column to match EPD
  dplyr::rename(practice_code = code_8) 

### Link and process data -----

# Aggregate all prescriptions by practice code
data_all <- setDT(data_epd)[,.(items = sum(items),total_quantity = sum(total_quantity),nic = sum(nic),`actual_cost`=sum(`actual_cost`)), by = .(practice_code)]

# Link and process
data_all <- data_all %>%
  # add IMD data
  left_join(data_imd, by = "practice_code") %>%
  # add list size data
  left_join(data_listsize, by = "practice_code") %>%
  # calculate items/1000 people
  mutate(items.per.1000 = 1000*items/total_list_size)  %>%
  # calculate proportion female
  mutate(prop.females = 100*total.females/total_list_size) %>%
  # calculate proportion older people (>55yr)
  mutate(prop.older = 11*total.older/total_list_size)

# Quantify missing data
missing.items <- data_all %>% filter(items<50)  %>% nrow()
missing.imd <- data_all %>% filter(items>=50) %>% filter(is.na(imd.score)) %>% nrow()
missing.listsize <- data_all %>% filter(items>=50) %>% filter(!is.na(imd.score)) %>% filter(is.na(total_list_size)) %>% nrow()
missing.listsize2 <- data_all %>% filter(items>=50) %>% filter(!is.na(imd.score)) %>% filter(!is.na(total_list_size)) %>% filter(total_list_size < 100) %>% nrow()

# Remove rows with missing data or low prescribing numbers 
data_all <- data_all %>% 
  # Remove rows with <50 items prescribed
  filter(items >= 50) %>%
  # Remove rows with no IMD value
  filter(!is.na(imd.score)) %>%
  # Remove rows with no list size data
  filter(!is.na(total_list_size)) %>%
  # Remove rows with low list size (<100 patients)
  filter(total_list_size >= 100)

# Print missing data information
print(paste(missing.items,"practices have fewer than 50 itmes prescribed over the time period"))
print(paste(missing.imd,"additional practices have no IMD 2019 score"))
print(paste(missing.listsize,"additional practices have no list size data"))
print(paste(missing.listsize2,"additional practices with fewer than 100 patients"))
print(paste("In total",missing.imd+missing.items+missing.listsize+missing.listsize2,"practices have been excluded before analysis"))

# Add ntile information
data_all <- data_all %>%
  # identify IMD quintiles
  mutate(imd.quintile = ntile(imd.score,5)) %>%
  # identify IMD tertiles
  mutate(imd.tertile = ntile(imd.score,3)) %>%
  # identify IMD deciles
  mutate(imd.decile = ntile(imd.score,10)) %>%
  # identify prescribing tertiles
  mutate(items.per.1000.tertile = ntile(items.per.1000,3)) %>%
  # identify prescribing deciles
  mutate(items.per.1000.decile = ntile(items.per.1000,10)) %>%
  # identify list size quintiles
  mutate(total.listsize.quintile = ntile(total_list_size,5)) %>%
  # identify female quintiles
  mutate(prop.females.quintile = ntile(prop.females,5)) %>%
  # identify older quintiles
  mutate(prop.older.quintile = ntile(prop.older,5)) %>%
  # keep required columns only
  select(practice_code, total_list_size, total.females, imd.score, imd.quintile, imd.tertile, imd.decile, items.per.1000, items.per.1000.tertile, total.listsize.quintile, prop.females, prop.females.quintile, prop.older, prop.older.quintile ,items) 

# Ensure ntiles are categorical (factors)
data_all$imd.quintile <- as.factor(data_all$imd.quintile)
data_all$imd.tertile <- as.factor(data_all$imd.tertile)
data_all$imd.decile <- as.factor(data_all$imd.decile)
data_all$total.listsize.quintile <- as.factor(data_all$total.listsize.quintile)
data_all$prop.females.quintile <- as.factor(data_all$prop.females.quintile)
data_all$prop.older.quintile <- as.factor(data_all$prop.older.quintile)
data_all$items.per.1000.tertile <- as.factor(data_all$items.per.1000.tertile)


# Add QOF prevalence data

# Read in prevalence data
data.qof <- read.csv("qof_prevalence_22_23.csv")

# For each comorbidity included, calculate proportion affected
data.copd <- data.qof %>%
  filter(GROUP_CODE == "COPD") %>%
  mutate(prop.copd = 100 * REGISTER / PRACTICE_LIST_SIZE) %>%
  select(PRACTICE_CODE, prop.copd)

data.dm <- data.qof %>%
  filter(GROUP_CODE == "DM") %>%
  mutate(prop.dm = 100 * REGISTER / PRACTICE_LIST_SIZE) %>%
  select(PRACTICE_CODE, prop.dm)

data.hf <- data.qof %>%
  filter(GROUP_CODE == "HF") %>%
  mutate(prop.hf = 100 * REGISTER / PRACTICE_LIST_SIZE) %>%
  select(PRACTICE_CODE, prop.hf)

# COmbine prevalence data and stratify into quintiles
data.prevalence <- data.copd %>%
  left_join(data.dm, by = "PRACTICE_CODE") %>%
  left_join(data.hf, by = "PRACTICE_CODE") %>%
  dplyr::rename(practice_code = PRACTICE_CODE) %>%
  mutate(prop.copd.quintile = ntile(prop.copd, 5)) %>%
  mutate(prop.dm.quintile = ntile(prop.dm, 5)) %>%
  mutate(prop.hf.quintile = ntile(prop.hf, 5)) 

# Join to main dataset
data_all <- data_all %>%
  left_join(data.prevalence, by = "practice_code")

# Ensure all new quintiles are factors
data_all$prop.copd.quintile <- as.factor(data_all$prop.copd.quintile)
data_all$prop.dm.quintile <- as.factor(data_all$prop.dm.quintile)
data_all$prop.hf.quintile <- as.factor(data_all$prop.hf.quintile)

### Plot data -----

# Function to give SE / CI of data, from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper%20functions
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Calculate 95% confidence intervals for Rx rate by IMD decile and save
df <- summarySE(data_all,measurevar = "items.per.1000",groupvars = "imd.decile")
df$lower.ci <- df$items.per.1000-df$ci #calculate lower and upper CIs
df$upper.ci <- df$items.per.1000+df$ci
dfA<-df[,c(1,2,3,7,8)] #subset data to save
names(dfA)[1] <- "IMD Decile" #rename columns
names(dfA)[2] <- "n"
names(dfA)[3] <- "Items per 1000 registered patients"
names(dfA)[4] <- "Lower 95% CI"
names(dfA)[5] <- "Upper 95% CI"
dfA$`Items per 1000 registered patients`<-round_any(dfA$`Items per 1000 registered patients`, 0.01)
dfA$`Lower 95% CI`<-round_any(dfA$`Lower 95% CI`, 0.01)
dfA$`Upper 95% CI`<-round_any(dfA$`Upper 95% CI`, 0.01)
write.csv(dfA,"Table 1. Average prescribing quantity by decile.csv",row.names = F)

# Set axis limits for bar chart
if(max(df$items.per.1000+df$ci)<150){   # set upper y-axis limit to nearest 10 if highest number is <150
  maxy=round_any(max(df$items.per.1000+df$ci), 10, f = ceiling)
}

if(max(df$items.per.1000+df$ci)>=150 && max(df$items.per.1000+df$ci)<=1500){   # set upper y-axis limit to nearest 100 if highest number is <1500
  maxy=round_any(max(df$items.per.1000+df$ci), 100, f = ceiling)
}

if(max(df$items.per.1000+df$ci)>1500){   # set upper y-axis limit to nearest 100 if highest number is >1500
  maxy=round_any(max(df$items.per.1000+df$ci), 1000, f = ceiling)
}

# Plot bar chart of prescribing rate by IMD decile
Cairo(file="Figure 1. Quantity prescribed per decile.png", 
      type="png",
      units="in", 
      width=5, 
      height=4, 
      pointsize=6, 
      dpi=1200)

ggplot(df, aes(x=imd.decile, y=items.per.1000)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black", size=0.4, fill=c("white","#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")) +
  geom_errorbar(aes(ymin=items.per.1000-ci, ymax=items.per.1000+ci),
                width=.3, size=0.4,                   # Width of the error bars, width of actual bar lines
                position=position_dodge())+
  xlab("\nIMD Decile") + ylab("Items prescribed\nper 1000 registered patients\n") +
  ylim(0,maxy)+
  scale_x_discrete(breaks = unique(df$imd.decile))+ 
  ggtitle("Average prescribing rate by practice IMD decile\n")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 10), axis.title = element_text(size= 10))

dev.off()

### Regression analysis ---

## Univariate

# By IMD quintile

summary(m1 <- glm(items.per.1000 ~ imd.quintile, family="quasipoisson", data=data_all))

# calculate robust standard errors (and related p values) to control for mild violation of the distribution assumption that the variance equals the mean
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

# calculate incident rate ratios and SEs, with CIs, using Delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) # exponentiate old estimates dropping the p values
rexp.est[, "Robust SE"] <- s #replace SEs with estimates for exponentiated coefficients
rexp.est
table2a <- as.data.frame(rexp.est) #ready to save
table2a$Estimate <- round_any(table2a$Estimate,0.01) #round to 2dp
table2a$LL <- round_any(table2a$LL,0.01)
table2a$UL <- round_any(table2a$UL,0.01)
table2a <- table2a[,-2] #remove SE column
table2a[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2a)[1] <- "IMD Quintile 1" #rename row 1
row.names(table2a)[2] <- "IMD Quintile 2" #rename row 2
row.names(table2a)[3] <- "IMD Quintile 3" #rename row 3
row.names(table2a)[4] <- "IMD Quintile 4" #rename row 4
row.names(table2a)[5] <- "IMD Quintile 5" #rename row 5

# NOTE estimates shows how many times more the IRR is compared to the reference group, or the % increase in IRR for every unit change in the predictor variable


# By list size quintile
summary(m1 <- glm(items.per.1000 ~ total.listsize.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2b <- as.data.frame(rexp.est) #ready to save
table2b$Estimate <- round_any(table2b$Estimate,0.01) #round to 2dp
table2b$LL <- round_any(table2b$LL,0.01)
table2b$UL <- round_any(table2b$UL,0.01)
table2b <- table2b[,-2] #remove SE column
table2b[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2b)[1] <- "List size Quintile 1" #rename row 1
row.names(table2b)[2] <- "List size Quintile 2" #rename row 2
row.names(table2b)[3] <- "List size Quintile 3" #rename row 3
row.names(table2b)[4] <- "List size Quintile 4" #rename row 4
row.names(table2b)[5] <- "List size Quintile 5" #rename row 5


# By older quintile
summary(m1 <- glm(items.per.1000 ~ prop.older.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2c <- as.data.frame(rexp.est) #ready to save
table2c$Estimate <- round_any(table2c$Estimate,0.01) #round to 2dp
table2c$LL <- round_any(table2c$LL,0.01)
table2c$UL <- round_any(table2c$UL,0.01)
table2c <- table2c[,-2] #remove SE column
table2c[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2c)[1] <- "Older age Quintile 1" #rename row 1
row.names(table2c)[2] <- "Older age Quintile 2" #rename row 2
row.names(table2c)[3] <- "Older age Quintile 3" #rename row 3
row.names(table2c)[4] <- "Older age Quintile 4" #rename row 4
row.names(table2c)[5] <- "Older age Quintile 5" #rename row 5



# By female quintile
summary(m1 <- glm(items.per.1000 ~ prop.females.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2d <- as.data.frame(rexp.est) #ready to save
table2d$Estimate <- round_any(table2d$Estimate,0.01) #round to 2dp
table2d$LL <- round_any(table2d$LL,0.01)
table2d$UL <- round_any(table2d$UL,0.01)
table2d <- table2d[,-2] #remove SE column
table2d[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2d)[1] <- "Female Quintile 1" #rename row 1
row.names(table2d)[2] <- "Female Quintile 2" #rename row 2
row.names(table2d)[3] <- "Female Quintile 3" #rename row 3
row.names(table2d)[4] <- "Female Quintile 4" #rename row 4
row.names(table2d)[5] <- "Female Quintile 5" #rename row 5



# By COPD quintile
summary(m1 <- glm(items.per.1000 ~ prop.copd.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2e <- as.data.frame(rexp.est) #ready to save
table2e$Estimate <- round_any(table2e$Estimate,0.01) #round to 2dp
table2e$LL <- round_any(table2e$LL,0.01)
table2e$UL <- round_any(table2e$UL,0.01)
table2e <- table2e[,-2] #remove SE column
table2e[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2e)[1] <- "COPD Quintile 1" #rename row 1
row.names(table2e)[2] <- "COPD Quintile 2" #rename row 2
row.names(table2e)[3] <- "COPD Quintile 3" #rename row 3
row.names(table2e)[4] <- "COPD Quintile 4" #rename row 4
row.names(table2e)[5] <- "COPD Quintile 5" #rename row 5


# By DM quintile
summary(m1 <- glm(items.per.1000 ~ prop.dm.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2f <- as.data.frame(rexp.est) #ready to save
table2f$Estimate <- round_any(table2f$Estimate,0.01) #round to 2dp
table2f$LL <- round_any(table2f$LL,0.01)
table2f$UL <- round_any(table2f$UL,0.01)
table2f <- table2f[,-2] #remove SE column
table2f[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2f)[1] <- "Diabetes Quintile 1" #rename row 1
row.names(table2f)[2] <- "Diabetes Quintile 2" #rename row 2
row.names(table2f)[3] <- "Diabetes Quintile 3" #rename row 3
row.names(table2f)[4] <- "Diabetes Quintile 4" #rename row 4
row.names(table2f)[5] <- "Diabetes Quintile 5" #rename row 5



# By HF quintile
summary(m1 <- glm(items.per.1000 ~ prop.hf.quintile, family="quasipoisson", data=data_all))
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table2g <- as.data.frame(rexp.est) #ready to save
table2g$Estimate <- round_any(table2g$Estimate,0.01) #round to 2dp
table2g$LL <- round_any(table2g$LL,0.01)
table2g$UL <- round_any(table2g$UL,0.01)
table2g <- table2g[,-2] #remove SE column
table2g[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
row.names(table2g)[1] <- "Heart failure Quintile 1" #rename row 1
row.names(table2g)[2] <- "Heart failure Quintile 2" #rename row 2
row.names(table2g)[3] <- "Heart failure Quintile 3" #rename row 3
row.names(table2g)[4] <- "Heart failure Quintile 4" #rename row 4
row.names(table2g)[5] <- "Heart failure Quintile 5" #rename row 5

# save univariate regression results
table2 <- rbind(table2a, table2b, table2c, table2d, table2e, table2f, table2g) # Combine results tables
write.csv(table2,"Table 2. Results of univariate regression.csv",row.names = T)

## Multivariate

summary(m1 <- glm(items.per.1000 ~ imd.quintile + total.listsize.quintile + prop.females.quintile + prop.older.quintile,  family="quasipoisson", data=data_all))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4), ~ exp(x5), ~ exp(x6), ~ exp(x7), ~ exp(x8), ~ exp(x9), ~ exp(x10), ~ exp(x11), ~ exp(x12), ~ exp(x13), ~ exp(x14), ~ exp(x15), ~ exp(x16), ~ exp(x17)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3]) 
rexp.est[, "Robust SE"] <- s 
rexp.est
table3 <- as.data.frame(rexp.est) #ready to save
table3$Estimate <- round_any(table3$Estimate,0.01) #round to 2dp
table3$LL <- round_any(table3$LL,0.01)
table3$UL <- round_any(table3$UL,0.01)
table3 <- table3[,-2] #remove SE column
table3[1, c(1,2,3)] <- c("Reference","","") #rename Q1 as reference
ref <- data.frame(Estimate="Reference",LL="",UL="") #Create a row for reference
table3 <- rbind(table3[1:5,],ref,table3[6:9,],ref,table3[10:13,],ref,table3[14:17,])
row.names(table3)[1] <- "IMD Quintile 1" # Rename rows
row.names(table3)[2] <- "IMD Quintile 2"
row.names(table3)[3] <- "IMD Quintile 3"
row.names(table3)[4] <- "IMD Quintile 4"
row.names(table3)[5] <- "IMD Quintile 5"
row.names(table3)[6] <- "List size Quintile 1" 
row.names(table3)[7] <- "List size Quintile 2" 
row.names(table3)[8] <- "List size Quintile 3" 
row.names(table3)[9] <- "List size Quintile 4" 
row.names(table3)[10] <- "List size Quintile 5" 
row.names(table3)[11] <- "Females Quintile 1" 
row.names(table3)[12] <- "Femalese Quintile 2" 
row.names(table3)[13] <- "Females Quintile 3" 
row.names(table3)[14] <- "Females Quintile 4" 
row.names(table3)[15] <- "Females Quintile 5" 
row.names(table3)[16] <- "Older age Quintile 1" 
row.names(table3)[17] <- "Older age Quintile 2" 
row.names(table3)[18] <- "Older age Quintile 3" 
row.names(table3)[19] <- "Older age Quintile 4" 
row.names(table3)[20] <- "Older age Quintile 5" 


# save multivariate regression results
write.csv(table3,"Table 3. Results of multivariate regression.csv",row.names = T)


# ## Calculate correlations and plot - prescribing by practice
# 
# df <- data_all # make name shorter to make coding for plots easier!
# 
# # correlation tests
# test1 <- cor.test(df$imd.score, df$items.per.1000, method = "spearman") # test for QUANTITY/1000 vs IMD = test2
# peeVal <- as.numeric(test1$`p.value`) # extract p value
# if(peeVal<0.001){peeVal="p < 0.001"} #set p value ranges
# if(peeVal<0.01 && peeVal>=0.001){peeVal="p < 0.01"}
# #if(peeVal>=0.01){peeVal=round_any(peeVal,0.01, f=round)}
# corVal <- round_any(as.numeric(test1$estimate),0.01,f=round)
# equals <- " = "
# 
# #set max y limit
# if(max(df$items.per.1000)<150){   # set upper y-axis limit to nearest 10 if highest number is <150
#   maxy=round_any(max(df$items.per.1000), 10, f = ceiling)
# }
# 
# if(max(df$items.per.1000)>=150 && max(df$items.per.1000)<=1500){   # set upper y-axis limit to nearest 100 if highest number is <1500
#   maxy=round_any(max(df$items.per.1000), 100, f = ceiling)
# }
# 
# if(max(df$items.per.1000)>1500){   # set upper y-axis limit to nearest 100 if highest number is >1500
#   maxy=round_any(max(df$items.per.1000), 1000, f = ceiling)
# }
# 
# Cairo(file="Figure 2. Scatterplot of prescribing rate vs IMD score.png", 
#       type="png",
#       units="in", 
#       width=5, 
#       height=4, 
#       pointsize=4, 
#       dpi=1200)
# 
# ggplot(df, aes(imd.score, items.per.1000))+
#   geom_point(size=0.5, stroke=0) + labs(x = "\nPractice IMD Score (2019)", y = "Items prescribed\nper 1000 registered females (15 to 54 years)\n")+
#   geom_smooth(method="lm", size=0.3)+
#   theme(text = element_text(size = 20))+
#   xlim(0,80)+
#   ylim(0,maxy)+
#   annotate(x=70, y=0.9*maxy, 
#            label=as.expression(bquote(rho == .(corVal))), 
#            geom="text", size=4)+
#   annotate(x=70, y=0.83*maxy, 
#            label=paste(peeVal), 
#            geom="text", size=4)+
#   theme(plot.title = element_text(hjust = 0.5, size = 12), axis.text = element_text(size = 10), axis.title = element_text(size= 10))
# 
# dev.off()
# 



# ## OpenData API [from https://github.com/nhsbsa-data-analytics/open-data-portal-api/blob/master/open-data-portal-api.R] -------------
# 
# # Define the url for the API call
# base_endpoint <- "https://opendata.nhsbsa.net/api/3/action/"
# package_list_method <- "package_list"     # List of data-sets in the portal
# package_show_method <- "package_show?id=" # List all resources of a data-set
# action_method <- "datastore_search_sql?"  # SQL action method
# 
# # Send API call to get list of data-sets
# datasets_response <- jsonlite::fromJSON(paste0(
#   base_endpoint, 
#   package_list_method
# ))
# 
# datasets_response$result # show datasets available