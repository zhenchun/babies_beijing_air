
library(readr)

###pm2.5 data of 2018

pm_dir= "C:/Users/zy125/Box Sync/Postdoc/BABIES/beijing_data/pm"

temp<-data.frame()

years<-c("2018","2019","2020")

pm2.5<-list()
pm10<-list()
so2<-list()
no2<-list()
ozone<-list()



colnam<-c("date", "hour", "type", "dongsi","tiantan","guanyuan","wanshouxigong","aotizhongxin","nongzhanguan","wanliu","beibuxinqu","zhiwuyuan",     
          "fengtaihuayuan", "yungang","gucheng","fangshan","daxing","yizhuang","tongzhou","shunyi","changping","mentougou", "pinggu", "huairou",       
          "miyun","yanqing","dingling","badaling","miyunshuiku","donggaocun", "yongledian","yufa","liulihe","qianmen","yongdingneimen","xizhimengbei",  
          "nansanhuan","dongsihuan")

for(year in years){
  
  setwd(file.path(pm_dir, year))
  
  files_pm=list.files(pattern=glob2rx("beijing_all_*.csv$"))
  files_other=list.files(pattern=glob2rx("beijing_extra_*.csv$"))
  pm2.5_all<-data.frame()
  pm10_all<-data.frame()
  
  for (f in files_pm){
    
    temp=read_csv(f, col_types = cols(date = col_date(format = "%Y%m%d"),hour = col_time(format = "%H")))
    temp[,1]
    
    n=1
    pm2.5_hour<-data.frame()
    pm10_hour<-data.frame()
    
    for(i in 1:24) {
      pm2.5_hour[i,1:38]<-temp[n,1:38]
      pm10_hour[i, 1:38]<-temp[n+2, 1:38]
      n=i*5+1
    } 
    
    pm2.5_all<-rbind(pm2.5_all, pm2.5_hour)
    pm10_all<-rbind(pm10_all, pm10_hour)
  }
    
    
  pm2.5[[year]]<-pm2.5_all
  pm10[[year]]<-pm10_all
  names(pm2.5[[year]])<-colnam
  names(pm10[[year]])<-colnam
  
  
  
  for(f in files_other){
    
  }

}


