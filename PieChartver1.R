
'Pie Chart ver1'

library(graphics)
library(RColorBrewer)
library(tidyverse)
library(mosaic)
library(stringr)
library(plotrix)
library(pdftools)



pdf_file <- 'PieChartver1.pdf'
cairo_pdf(pdf_file,bg = 'grey98',height = 3.75,width = 9)

par(omi = c(0.5,0.5,1,0.5),
    mai = rep(0,4),
    xpd = TRUE,
    mfcol = c(1,2),
    family = 'Lato Light')

plot(1:5,type = 'n',axes = FALSE,xlab = '',ylab = '',
     xlim = c(1,5),ylim = c(1,10))

mySeats <- c(51,54,61,222,226)
myDes <- c(mySeats,'')
mySlices <- 73*mySeats/sum(mySeats)
myValues <- c(mySlices,50)
myDisc <- 100 
myColors <- c('white','white','black','white','white')

mySemiCircles <- floating.pie(xpos = 3,ypos = 1,myValues,border = NA,
                              radius = 1.9,xpd = FALSE,
                              col = c('green','pink','yellow','red','black',par('bg')))

pie.labels(3,1,angles = mySemiCircles,myDes,bg = NA,border = NA,radius = 1.25,
           cex = 1.15,col = myColors)

floating.pie(3,1,myDisc,border = NA,col = par('bg'),radius = 0.7,xpd = FALSE)

mtext('16th Germany Bundestag',side = 3,line = 0,adj = 0.5,font = 2.5,cex = 1.15)

par(xpd = TRUE)
legend(1,0.5,legend = c('Union (CDU/CSU)','Socialist Party (SPD)','Free Democratic Party (FDP)',
                        'Left Party (Die Linke)','Alliance',"90/The Green (Bundnis 90/Die Grumen)"),
       border = FALSE,pch = 15,col = c('black','red','yellow','pink','green'),bty = 'n',cex = 0.7,
       ncol = 3,xpd = NA)
par(xpd = FALSE)

plot(1:5,type = 'n',axes = FALSE,xlab = '',ylab = '',xlim = c(1,5),ylim = c(1,10))
mySeats <- c(68,76,93,146,237)
myDes <- c(mySeats,'')
mySlices <- 50*mySeats/sum(mySeats)
myValues <- c(mySlices,50)
myDisc <- 100


semiCircles <- floating.pie(3,1,myValues,border = NA,radius = 1.9,xpd = FALSE,
                            col = c('green','pink','yellow','red','black',par('bg')))

pie.labels(3,1,mySemiCircles,myDes,bg = NA,border = NA,radius = 1.25,cex = 1.15,col = myColors)

floating.pie(3,1,myDisc,border = NA,col = par('bg'),radius = 0.7,xpd = FALSE)

mtext('17th Germany Bundestag',3,line = 0,adj = 0.5,font = 2.5,cex = 1.15)

# Titling;;;

mtext('Seat distribution in Germany Bundestag',side = 3,line = 3,
      adj = 0,family = 'Lato Black',outer = TRUE,cex = 1.65)
mtext('Source: Dantri.com.vn',side = 1,line = 1,adj = 1,cex = 0.7,
      font = 3,outer = TRUE)

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




