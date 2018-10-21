#################Cloudy Nights Scrape#############
#################9/19/18 by cms####################


#Objective:  Scrape all the Telescope Eyepiece data going back to the beginning.  Then aggregate the data by 
#month and run a count and also pages viewed.  

library(dplyr)
library(rvest)
library(xml2)



#######################################################################################################
##############################################Functions for Pulling Data###############################
#######################################################################################################
#Pages for urls (pages)
pages <- seq(0, 5525, 25) 

#Create the URls
urls<- paste("https://www.cloudynights.com/index.php?app=classifieds&do=view_category&category_id=13&sort_key=date_added&sort_order=desc&filter=0&st=",pages, sep = "")

#Function for the Title of the Telescope eyepiece 
func_for_title <- function(url){
 url <- read_html(url)
title <-  html_nodes(url,".ipsType_subtitle a")%>%
   html_text()
}
 
#Use lapply to run the urls through the function
# titles <- lapply(urls, func_for_title)
#Unlist 
# titles <- unlist(titles)
#Then create df.
# titles_final <- data.frame(titles)
#write to csv
# write.csv(titles_final, "titles.csv")

#Function for pulling the number of views.  There is some extrenuous data coming through.
#Updated Dates are pulling in as well.  I'll clean that seperately using regex.
func_for_Views <- function(url){
  ref <- read_html(url)
 views <- ref%>%
  html_nodes(css= ".ipsType_small+ .ipsType_small")%>%
  html_text()
 return(views)
}

# views <- lapply(urls, func_for_Views)
# views1 <- unlist(views)
# views1 <- data.frame(views1)
# 
# write.csv(views1, "views.csv")


#Function for when the Post was added.
func_for_date <- function(url){
  url <- read_html(url)
    date <- url%>%
      html_nodes(".item_info .ipsType_small:nth-child(1)")%>%
      html_text()
}

# df3 <- lapply(urls, func_for_date)
# df4 <- unlist(df3)
# df <- data.frame(df4)
# 
# write.csv(df, "dates.csv")


###############################################Data Cleaning###################################


#Take out updated columns in Views
#Use grepl to find out which rows contain updated.
filt <- grepl("Updated:", views1$views1)

#Filter out such rows
Views_final <- views1[filt_out == F,]

#take out Views and convert to Numeric

Views_final <- gsub("\\w+:\\s+","", Views_final)
Views_final <- str_trim(Views_final)
Views_final <- as.numeric(Views_final)


######Clean up dates column
#Get rid of everything after comma
df_date <- gsub("(\\,.*)", "", df$df4)
df_date <- str_trim(df_date, side = c("right"))
df_date <- as.character(df_date)
df_date <- data.frame(date = df_date, stringsAsFactors = F)

#loop that changes today and yesterday to dates
for(i in 1:nrow(df_date)){
  if(df_date$date[i] == "Today"){
    df_date$date[i] <- "22 Sep 2018"
  } else {
    if(df_date$date[i] == "Yesterday"){
      df_date$date[i] <- "21 Sep 2018"
    }
  }
}

library(lubridate)
#Change to date using dplyr
df_date$date <- dmy(df_date$date)

#Bind all the rows. 

nrow(df_date)
nrow(titles_final)
nrow(Views_final)

final_df <- cbind(df_date, titles_final, Views_final)

write.csv(final_df, "final_df_combined.csv")




  

