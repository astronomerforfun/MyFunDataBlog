#####################Scraping Tables Messier Objects Wiki############
#################################9/19/18 by cs########################


library(rvest)
library(dplyr)
library(xml2)


#Read in html site
messier <- read_html("https://en.wikipedia.org/wiki/List_of_Messier_objects")

#This doesn't work...  
z <- messier%>%
  html_nodes(".mw-parser-output")%>%
  html_table(fill = TRUE)

#Use this.  Basically it pulls all the tables from the object (no nodes needed)
z <- messier%>%
  html_table(fill = TRUE)
#convert to Df.
z <- as.data.frame(z)

#Step Process for image downloads; 1) download img css; 2) extract link; 
#clean link, 4) test by copy and pasting in browser, 5) create function
#Downloading img path
image <- messier%>%
  html_nodes("img")

#Extract the path
image.url <- image%>%html_attr("src")

#Add http:// in this case.
urls_for_img <- function(x){
final_urls <- paste("http:",x, sep = "")
return(final_urls)
}

final <- urls_for_img(image.url)

final
#See it works then create function.



download.file(final[2], "final.jpg", mode="wb")
download.file(y, "test.jpg", mode = "wb")
