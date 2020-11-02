
library(readr)

###pm2.5 data of 2018

pm_dir= "C:/Users/zy125/Box Sync/Postdoc/BABIES/beijing_data/pm"

temp<-data.frame()

years<-c("2018","2019","2020")

pm2.5<-list()
colnam<-c("date", "hour", "type", sta_info$name)

for(year in years){
  
  setwd(file.path(pm_dir, year))
  
  files=list.files(pattern=glob2rx("beijing_all_*.csv$"))
  pm2.5_all<-data.frame()
  
  
  
  for (f in files){
    
    temp=read_csv(f)
    
    n=1
    pm2.5_hour<-data.frame()
    
    for(i in 1:24) {
      pm2.5_hour[i,1:38]<-temp[n,1:38]
      n=i*5+1
    } 
    
    pm2.5_all<-rbind(pm2.5_all, pm2.5_hour)
  }
    
    
  pm2.5[[year]]<-pm2.5_all  
  names(pm2.5[[year]])<-colnam

}


