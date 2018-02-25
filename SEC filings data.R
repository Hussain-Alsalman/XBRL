
#Get the XML file. 
# Parse the XBRL files using the XBRL library  
# Do Queries on the Tables exists 
# Formulize Queries to get the data for other periods


#Installing the required packages. 
#install.packages(c("XBRL", "finreportr", "stringr"))
library("XBRL")
library("finreportr")
library("stringr")

#Setting up the downloading method
options(download.file.method = "curl")


#getting the available Annual reports for the JP Morgan  bank 
JPM.annual<-AnnualReports("jpm", foreign = FALSE)

#Extracting the CIK number 
cik <- as.character(as.numeric(str_split(JPM.annual$accession.no[5], "-")[[1]][1]))

#Listing all the LINK available for the Annual reports 
cbind(paste0("https://www.sec.gov/Archives/edgar/data/",cik,"/", JPM.annual$accession.no, "-index.html"), JPM.annual$filing.date)


url <- "https://www.sec.gov/Archives/edgar/data/19617/000001961714000289/jpm-20131231.xml"

old_o <- options(stringsAsFactors = FALSE)
xbrl_data <- xbrlDoAll(url, delete.cached.inst = TRUE)
options(old_o)

library(dplyr)
library(tidyr)

table(xbrl_data$calculation$contextElement)

str(xbrl_data$calculation, max.level = 1, vec.len = 0)

xbrl_data$presentation[sample(1:200,5),2:3]
xbrl_data$presentation %>% inner_join(xbrl_data$element, by =c("fromElementId"= "elementId")) %>%
  inner_join(xbrl_data$element, by = c("toElementId"="elementId")) %>% View()


xbrl_data$role %>% filter(grepl("Credit", roleId))
role_id <- "http://www.jpmorganchase.com/role/CreditRiskConcentrationsDetails"

pres <-
  xbrl_data$presentation %>%
  filter(roleId == role_id) %>% 
  mutate(order = as.numeric(order))

pres_df <- 
  pres %>%
  anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
  select(elementId = fromElementId)

# breadth-first search
while({
  df1 <- pres_df %>%
    na.omit() %>%
    left_join(pres, by = c("elementId" = "fromElementId")) %>%
    arrange(elementId, order) %>%
    select(elementId, child = toElementId) ;
  nrow(df1) > 0
}) 
  
{
  # add each new level to data frame
  pres_df <- pres_df %>% left_join(df1, by = "elementId") 
  names(pres_df) <-  c(sprintf("level%d", 1:(ncol(pres_df)-1)), "elementId")
}


# add last level as special column (the hierarchy may not be uniformly deep)
pres_df["elementId"] <- 
  apply( t(pres_df), 2, function(x){tail( x[!is.na(x)], 1)})
pres_df["elOrder"] <- 1:nrow(pres_df) 


str(pres_df, vec.len = 1 )


# join concepts with context, facts
pres_df_num <-
  pres_df %>%
  left_join(xbrl_data$fact, by = "elementId") %>%
  left_join(xbrl_data$context, by = "contextId") %>%
  filter(!is.na(dimension1)) %>%
  filter(!is.na(endDate)) %>%
  select(elOrder, contains("level"), elementId, fact, decimals, endDate) %>%
  mutate( fact = as.numeric(fact) * 10^as.numeric(decimals)) %>%
  spread(endDate, fact ) %>%
  arrange(elOrder)

x_labels <-
  xbrl_data$presentation %>%
  filter(roleId == role_id) %>%
  select(elementId = toElementId, labelRole = preferredLabel) %>%
  semi_join(pres_df_num, by = "elementId") %>%
  left_join(xbrl_data$label, by = c("elementId", "labelRole")) %>%
  filter(lang == "en-US") %>%
  select(elementId, labelString)

x_calc <- xbrl_data$calculation %>%
  filter(roleId == role_id) %>%
  select(elementId = fromElementId, calcRoleId = arcrole) %>%
  unique()


balance_sheet_pretty <- pres_df_num %>%
  left_join(x_labels, by = "elementId") %>%
  left_join(x_calc, by = "elementId") %>%
  select(labelString, contains("2013"), contains("2012"), calcRoleId)




jpm2015

jpm2011
jpm2014<-
  xbrl_data$element %>%
  filter(grepl("CreditExposure", elementId) & type == "xbrli:monetaryItemType") %>%
  inner_join(xbrl_data$fact, by = "elementId")  %>%
  inner_join(xbrl_data$context, by = "contextId") %>%
  dplyr::mutate(amount = as.numeric(fact)*10^(as.numeric(decimals))) %>%
  #dplyr::mutate(catagory = str_extract(pattern ="jpm[:alpha:]+Member", string =contextId)) %>%
  inner_join(xbrl_data$definition, by = c("elementId"= "toElementId") ) %>%
  #inner_join(xbrl_data$presentation, by = "roleId") %>%
  select(contextId,value1, amount, startDate, endDate, periodType, arcrole) %>% dplyr::arrange(value1) %>% 
  split(.$periodType)


dim(xbrl_data$calculation)
dim(xbrl_data$presentation)
dim(xbrl_data$definition)

dim(xbrl_data$context)
table(xbrl_data$role$type)
str(
xbrl_data$role %>% filter(type == "Disclosure") %>% inner_join(xbrl_data$presentation, by = "roleId")
, vec.len = 3
)
table(xbrl_data$role$order.x)
str(xbrl_data$role, max.level = 1)
sum(jpm2013$instant$amount[2:27])



dim(jpm2014$instant)

  
24*2

asg<-grep(x =jpm2011$contextId, pattern = "[Concenteration[:alnum:]Member]", perl = TRUE, value = TRUE)
gsub("I2011Q4_jpm_ConcentrationRiskByPortfolioSegmentAxis_us-gaap_CommercialPortfolioSegmentMember_us-gaap", replacement = "", jpm2011$contextId)


test.string <- "I2010Q4_jpm_ConcentrationRiskByPortfolioSegmentAxis_us-gaap_CommercialPortfolioSegmentMember_us-gaap_ConcentrationRiskByTypeAxis_jpm_ReceivablesFromCustomersCreditRiskConcentrationMember"

str_extract(pattern ="jpm_[:alpha:]+Member", string = test.string)

jpm2015$instant
library("ggplot2")

xbrl_data$role %>% inner_join(xbrl_data$calculation, by ="roleId") %>% filter(grepl("Exposure", fromElementId))
dim(xbrl_data$presentation)
table(xbrl_data$definition$contextElement)

