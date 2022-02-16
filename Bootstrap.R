library(boot)
library(dplyr)
library(magrittr)
library(tidyverse)

#import data
df <- read.csv2("Grupa.4.csv")

#choosing needed columns
df <- select(df, RecordNo, S14A, S14B)

#changing columns names
df <- setNames(df, c("RecordNo","powierzchnia2021", "powierzchnia2022"))

#removing whitespaces
df_1 = as.data.frame(apply(df,2,function(x)gsub('\\s+', '',x)))

#swiccthing "," to "."
df$powierzchnia2021 = as.numeric(as.character(gsub(",", ".", gsub("\\.", "", df_1$powierzchnia2021))))
df$powierzchnia2022 = as.numeric(as.character(gsub(",", ".", gsub("\\.", "", df_1$powierzchnia2022))))

#creating "change" column
df <- mutate(df, "change" = df$powierzchnia2021 - df$powierzchnia2022)

#estimating function
foo <- function(data, indices){
  dt<-data[indices,]
  c(
    mean(dt[,4]),
    mean(dt[,2]),
    mean(dt[,3])
  )
}

#seed for reproductive results
set.seed(12345)

# inspecting results
myBootstrap <- boot(df, foo, R=1000)
View(myBootstrap)
#foo function calls
View(myBootstrap$t)

#myBootstrap$t to csv
bsdf <- myBootstrap$t
write.csv(bsdf, "myBootstrap.csv")

#t0 in comparison to original dataset
mean(df$powierzchnia2021)
mean(df$powierzchnia2022)
mean(df$change)
head(myBootstrap$t0)

###change mean
#histogram
plot(myBootstrap, index=1)
#confidence interval 
boot.ci(myBootstrap, index=1)


###next year mean
#histogram
plot(myBootstrap, index=3)
#CI 
boot.ci(myBootstrap, index=3)