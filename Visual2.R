


# Data Visualization 
library(graphics)
library(tidyverse)

# Create labels!!!
myC_v159 <- "A working mother can establish just as warm and\nsecure an environment as a non-working mother"
myC_v160 <- "A pre-school child is likely to suffer if\nhis or her mother is working"
myC_v161 <- "A job is alright, but what most women\nreally want is a home and children"
myC_v162 <- "Being a housewife is just as fulfilling as\nworking"
myC_v163 <- "Having a job is the best way for a woman\nto be independent"
myC_v164 <- "Both the husband and wife should contribute\nto the family income"
myC_v165 <- "In general, fathers are as well suited to\nlook after their children as women"
myC_v166 <- "Men should take as much responsibility\nas women for their household and children"
myNames <- c(myC_v165, myC_v164, myC_v163, myC_v162, myC_v161, myC_v160, myC_v159)

df <- c(70.1,84.7,84.8,35.5,33.1,47.2,76.4)
df2 <- vector('double',length(df))
df2[which(df == 84.8)] <- df[which(df == 84.8)]


###---------------------------------------------------------------------------------
###---------------------------------------------------------------------------------
###---------------------------------------------------------------------------------

pdf_file <- '2ndchart.pdf'
cairo_pdf(pdf_file,bg = 'grey98',width = 13,height = 10.5)

par(omi = c(0.65,0.75,1.25,0.75),
    mai = c(0.9,3.85,0.55,0),
    lheight = 1.15,
    family = 'Lato Light',
    las = 1)

bp <- barplot(df,names.arg = FALSE,
              horiz = TRUE,
              border = NA,
              xlim = c(0,100),
              col = 'grey',
              axes = FALSE,
              family = 'Lato')

myColor <- rgb(255,0,210,maxColorValue = 255)
rect(0,-0.1,20,8.6,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
rect(20,-0.1,40,8.6,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
rect(40,-0.1,60,8.6,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)
rect(60,-0.1,80,8.6,col = rgb(191,239,255,120,maxColorValue = 255),border = NA)
rect(80,-0.1,100,8.6,col = rgb(191,239,255,80,maxColorValue = 255),border = NA)

bp <- barplot(df2,
              names.arg = FALSE,
              horiz = TRUE,
              border = NA,
              xlim = c(0,100),
              col = myColor,
              axes = FALSE,
              add = TRUE)

# Other Elements

for (i in 1:length(myNames)) {
  
  if (i == 3) {
    
    myFont <- 'Lato Bold'
    
  }else{
    
    myFont <- 'Lato Light'
    
  }
  
  text(-3,bp[i],myNames[i],xpd = TRUE,adj = 1,family = myFont,cex = 1)
  text(10,bp[i],df[i],family = myFont,cex = 1.25,col = ifelse(i == 3,'white','black'))

}

arrows(50,-0.1,50,8.8,lwd = 1.5,length = 0,xpd = TRUE,col = 'skyblue3')
arrows(50,-0.25,50,-0.1,lwd = 5,length = 0,xpd = TRUE)
arrows(50,8.8,50,8.95,lwd = 5,length = 0,xpd = TRUE)

text(48,8.9,'Majority',adj = 1,xpd = TRUE,cex = 0.9,font = 3)
text(52,8.9,'50%',adj = 0,xpd = TRUE,cex = 0.9,font = 3,family = 'Lato Bold')
text(100,8.9,'All values in percent',adj = 1,xpd = TRUE,cex = 0.9,font = 3)
mtext(seq(0,100,by = 20),at = seq(0,100,by = 20),1,line = 0.75)

# Titles for margins

mtext('It is often said that attitudes towards gender roles are changing',
      side = 3,line = 2.2,adj = 0,cex = 1.8,family = 'Lato Black',outer = TRUE)
mtext('The answers are:',side = 3,line = 0,adj = 0,cex = 1.5,outer = TRUE)
mtext('Source: dantri.com.vn\nDesign: Phan Tien Dung\nUniversity of Tuebingen,Germany',
      side = 1,line = 0,adj = 1,cex = 0.95,outer = TRUE,font = 3)


dev.off()






































