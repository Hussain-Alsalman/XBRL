#Installing the required packages. 
#install.packages(c("XBRL", "finreportr", "stringr"))
library("XBRL")
library("finreportr")
library("stringr")
#============================ Setting up some parameters =========================

#Setting up the downloading method
options(download.file.method = "curl")

#Saving our current settings
original.options <-options()

#Setting up the new options
new.options <- options(stringsAsFactors = FALSE)

#===================Getting The links for our specified company  ==================

#Inital Parameters
Comp.tick <- "AAPL"

#Getting the available Annual reports for the JP Morgan  bank 
Annuals.list<-AnnualReports(Comp.tick, foreign = FALSE)
Annuals.list

#Selecting the Type of record we would like to analyze 
Rec.no <- 1

#Extracting the CIK number 
cik <- as.character(as.numeric(str_split(Annuals.list$accession.no[Rec.no], "-")[[1]][1]))

#Listing all the LINK available for the Annual reports 
cbind(paste0("https://www.sec.gov/Archives/edgar/data/",cik,"/", Annuals.list$accession.no, "-index.html"), Annuals.list$filing.date)

#Selecting the URL we are interested in
url <- "https://www.sec.gov/Archives/edgar/data/320193/000032019317000070/aapl-20170930.xml"



#=================== Starting the XBRL files Analysis =========================

#Analzying the XBRL files ### THIS MAY TAKE LONG TIME ####
xbrl_data <- xbrlDoAll(url, delete.cached.inst = TRUE)


#Loading Some Data minupliating libraries 
library(dplyr)
library(tidyr)

#================== Experimenting 

#Display the type of statments we have 
#install.packages("htmlTable")  #----uncomment if package is not installed
library("htmlTable")
#Filtering only the statements in the XBRL document
filtered.data <- xbrl_data$role %>% filter(xbrl_data$role$type == "Statement")

#Exploring all available statements
html.table <- htmlTable::htmlTable(data.frame(Statements =  with(filtered.data, paste("<h3>",definition,"</h3>", "</br><p>", roleId, "\n<p/>"))),
                                  align = "l",
                                  rname = FALSE,
                                  css.cell = "pedding-bottom: .2em; pedding-top: .2em;")

role_Id <- "http://www.apple.com/role/ConsolidatedBalanceSheets"
# Visualizing The calculation stracture of the financial statements
library(igraph)
doc.graph <-xbrl_data$calculation [which(xbrl_data$calculation$roleId == role_Id ),c("fromElementId", "toElementId")]
doc.viz <- graph.data.frame(doc.graph)
tkplot(doc.viz, vertex.label.color ="darkGreen")

xbrl_data$calculation [which(xbrl_data$calculation$roleId == role_Id ),]
#==== Restoring the original setting of certain parameters
#Restoring original
options(original.options)


#================== Experimenting ==========================
xbrl_data$element %>%
  filter(elementId=="us-gaap_CostOfGoodsAndServicesSold" ) %>%
  left_join(xbrl_data$fact, by =  "elementId") %>% left_join(xbrl_data$context, by ="contextId") %>%
  left_join(xbrl_data$label, by = "elementId") %>%  left_join(xbrl_data$unit, by ="unitId")

xbrl_data$fact %>% filter(elementId == "us-gaap_CostOfGoodsSold")

xbrl_data$element %>% filter(grepl("Sales", elementId) & type =="xbrli:monetaryItemType" & periodType == "duration") %>%
  select(elementId)
#===========================================================
