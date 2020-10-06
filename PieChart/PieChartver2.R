

'Pie Chart ver2'
library(tidyverse)
library(graphics)
library(stringr)
library(readr)
library(readxl)
library(RColorBrewer)
library(pdftools)


pdf_file <- 'PieChartver2.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 15,height = 11)
par(omi = c(rep(0.5,2),0.75,0.5),
    mai = rep(0.1,4),
    family = 'Lato Light',
    las = 1)


df <- read_excel('Healthcare_costs.xlsx',sheet = 1) 
attach(df)
n <- nrow(df)

myFactor <- max(sqrt(Acosts60))/0.8

plot.new()
myC0 <- rep(NA,n)
myColors <- brewer.pal(n,'Set3')

for (i in 1:n) {
  
  par(new = TRUE)
  r <- col2rgb(myColors[i])[1]
  g <- col2rgb(myColors[i])[2]
  b <- col2rgb(myColors[i])[3]
  myC0[i] <- rgb(r,g,b,190,maxColorValue = 255)
  myValue <- format(Total60/1000000,digits = 1)
  myTotal <- str_c(Disease,': ',myValue,'Mio. $',sep = '')
  
  if (Acosts60[i] == max(Acosts60)) {
    
    myDes <- myTotal
    
  } else {
    
    myDes <- NA
    
  }
  
  
  pie(Patients60,border = NA,radius = sqrt(Acosts60[i])/myFactor,
      col = myC0,labels = myDes,cex = 1.8)
  
  par(new = TRUE)
  r <- col2rgb(myColors[i])[1]
  g <- col2rgb(myColors[i])[2]
  b <- col2rgb(myColors[i])[3]
  myC0[i] <- rgb(r,g,b,maxColorValue = 255)
  
  pie(Patients60,border = NA,radius = sqrt(Pcosts60[i])/myFactor,
      col = myC0,labels = NA)
  myC0 <- rep(NA,n)
  
}

mtext('The Cost of Getting Sick',side = 3,line = -1,adj = 0,cex = 3.5,family = 'Lato Black',outer = TRUE)
mtext('The medical Expenditure Panel Survey. Age: 60, Total Cost: 41.4 Mio US $.',side = 3,line = -3.6,
      adj = 0,cex = 1.75,outer = TRUE)
mtext('Inside: Personal Cost. Outside: Insurer Costs.',side = 1,line = 0,adj = 0,cex = 1.65,outer = TRUE,
      font = 3)
mtext('Source: dantri.com.vn. Redesigned by: Phan Tien Dung.',side = 1,line = 0,adj = 1,cex = 1.65,
      outer = TRUE,font = 3)

dev.off()


# Convert image form;

pdf_convert(
  
  
  pdf = pdf_file,
  format = 'png',
  pages = NULL,
  filenames = NULL,
  dpi = 72,
  antialias = TRUE,
  opw = '',
  upw = '',
  verbose = TRUE
  
  
)

rm(list = ls())
cat('\f')






