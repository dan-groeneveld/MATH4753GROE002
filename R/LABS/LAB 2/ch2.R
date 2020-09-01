# This is a little advanced at the beginning. Later is easy.
# To use all of this code you will need to install packages
# readxl, stringr, purrr, plotrix, qcc, ggplot2


dird = "D:/MATH4773-5773/DATA/Excel/"
library(readxl)

files = list.files(dird)
files

myconvert = function(xl) {
  if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
  v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
  }
  else{
    v = NA
  }
  v
}

v  = purrr::map(files, ~myconvert(.x))
l <- stringr::str_length(files)
l
newnames <- stringr::str_sub(files,1,l-4)
newnames
names(v) <- newnames
v[1]

############ FATAL part 1

fatal <- myconvert("FATAL.XLS")
fatal$Cause

freq = table(fatal$Cause)
addmargins(freq)
prop <- prop.table(freq)
prop
addmargins(prop)
rbind(freq,prop)



## plots

barplot(freq)


pie(freq)

plotrix::pie3D(as.vector(freq),labels = names(freq),explode = 0.2,theta = 1)

windows()
qcc::pareto.chart(freq)

### FRECKLE

freckle <- myconvert("FRECKLE.xls")
freckle$`F-INDEX`
stem(freckle$`F-INDEX`)

#### FATAL part 2

fatal <- myconvert("FATAL.XLS")
obj = scale(fatal$Fatalities)
f<-obj[,1]
coll = ifelse(f>3, "Red",ifelse(f>2, "Blue","Black"))
plot(f, type = "b", col = coll, pch =19, cex =2)
abline(h = 3)

## ggplot FATAL

# ggplot works on data frames
library(ggplot2)
df = as.data.frame(obj)
names(df) = "z"
g <- ggplot(df) + geom_point(aes(y=z, x=seq_along(z)), color = coll) + xlab("Index") + ylab("Z standardization")
g
