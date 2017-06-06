library(xlsx)
library(gmapsdistance)
Data <- read.xlsx(file = "C:/Users/whuang67/downloads/zip_driving_distance.xlsx",
                  sheetName = "temp1")

Data <- load(file = "C:/Users/whuang67/downloads/ZipCode.RData")


for(i in 1:nrow(Data)){
  if(Data[i, 1] == "06431" & Data[i, 2] == "06431"){
    Data$Distance[i] = NA
    Data$Time[i] = NA
    
  } else if(Data[i, 1] == "06431" & Data[i, 2] != "06431"){
    abc = gmapsdistance(origin = "06828",
                        destination = as.character(Data[i,2]),
                        mode = "driving")
    Data$Distance[i] = abc$Distance
    Data$Time[i] = abc$Time
    
  } else if(Data[i, 1] != "06431" & Data[i, 2] == "06431"){
    abc = gmapsdistance(origin = as.character(Data[i, 1]),
                        destination = "06828",
                        mode = "driving")
    Data$Distance[i] = abc$Distance
    Data$Time[i] = abc$Time
    
  } else{
    abc = gmapsdistance(origin = as.character(Data[i,1]),
                        destination = as.character(Data[i,2]),
                        mode = "driving")
    Data$Distance[i] = abc$Distance
    Data$Time[i] = abc$Time
    
  }
  print(i)
}


which(is.na(Data$Distance) ==TRUE & (Data$zip != "06431" & Data$zip2 != "06431"))

write.xlsx(x = Data,
           file = "C:/users/whuang67/downloads/zip_driving_distance_time.xlsx",
           sheetName = "Sheet1",
           col.names = TRUE,
           row.names = FALSE)
Data$zip <- as.character(Data$zip)
Data$zip2 <- as.character(Data$zip2)
write.csv(x = Data,
          file = "C:/users/whuang67/downloads/zip_driving_distance_time.csv",
          row.names = TRUE)

