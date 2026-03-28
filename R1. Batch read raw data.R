library(dplyr)
library(ggplot2)
library(ggsci)

### Set the file directory and read
Risk <- data.frame()
path <- "G:\\Smooth progress in research\\Good health\\Peace and joy" # The location of the folder containing the GBD table
fileName = dir(path)
fileName
Risk <- data.frame()

for (k in 1:length(fileName)) {
  data = vroom::vroom(file = paste(path,fileName[k],sep="\\"))
  Risk = rbind(Risk,data)
}

colnames(Risk)
unique(Risk$measure)
unique(Risk$rei)

Risk <- Risk %>%
  mutate(measure = gsub("DALYs \\(Disability-Adjusted Life Years\\)", "DALYs", measure))

Risk <- Risk %>%
  mutate(measure = gsub("YLDs \\(Years Lived with Disability\\)", "YLDs", measure))

Risk <- Risk %>%
  mutate(measure = gsub("YLLs \\(Years of Life Lost\\)", "YLLs", measure))
