
library(dplyr)

setwd('c:/Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/prl_data/')

# 
file_list <- list.files(pattern = ".csv$")

# 
data_list_prime <- lapply(file_list, read.csv, stringsAsFactors = FALSE)


nSubjects <- as.numeric(length(data_list_prime))
nTrails <- as.numeric(sum(data_list_prime[[1]]$selected_image%in%c('image1','image2','None')))
x <- data_list_prime[[1]]$selected_image
x <- x[x!='']

#
subid_list <- vector(length = nSubjects)
for (t in 1:nSubjects) {
  
  id <- data_list_prime[[t]][["participant"]]
  subid_list[t] <- id[1]
  
}

data_data <- array(NA,dim = c(length(data_list_prime),length(x),2))

for (i in 1:nSubjects) {
  
  choice <- data_list_prime[[i]]$selected_image
  choice <- choice[choice!='']
  # 
  choice[choice == "image1"] <- "1"
  # 
  choice[choice == "image2"] <- "2"
  # 
  choice[choice == "None"] <- NA
  choice <- na.omit(choice)
  data_data[i,,1] <- as.numeric(choice)

  
  reward <- data_list_prime[[i]]$reward_result
  reward <- reward[reward!='']
  reward<- na.omit(reward)
  #  
  reward[reward=="no_react"] <- NA
  reward<- na.omit(reward)
  data_data[i,,2] <- as.numeric(reward)
  
}

#
setwd('c:/Users/MYQ20/Desktop/paper1/submit_depression and anxiety/code and data/')
print(data_data)
save(data_data,file="behavior_data.Rdata")
save(subid_list,file = 'id_list.Rdata')
