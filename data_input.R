
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
co<-list()

sta_names<-c("dongsi","tiantan","guanyuan","wanshouxigong","aotizhongxin","nongzhanguan","wanliu","beibuxinqu","zhiwuyuan",     
             "fengtaihuayuan", "yungang","gucheng","fangshan","daxing","yizhuang","tongzhou","shunyi","changping","mentougou", "pinggu", "huairou",       
             "miyun","yanqing","dingling","badaling","miyunshuiku","donggaocun", "yongledian","yufa","liulihe","qianmen","yongdingneimen","xizhimengbei",  
             "nansanhuan","dongsihuan")

colnam<-c("date", "hour", "type", sta_names)

for(year in years){
  
  setwd(file.path(pm_dir, year))
  
  files_pm=list.files(pattern=glob2rx("beijing_all_*.csv$"))
  files_other=list.files(pattern=glob2rx("beijing_extra_*.csv$"))
  pm2.5_all<-data.frame()
  pm10_all<-data.frame()
  so2_all<-data.frame()
  no2_all<-data.frame()
  ozone_all<-data.frame()
  co_all<-data.frame()
  
  for (f in files_pm){
    
    temp=read_csv(f, col_types = cols(date = col_date(format = "%Y%m%d"),hour = col_time(format = "%H")))
    
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
  
  
  
  for(fi in files_other){
    temp1=read_csv(fi, col_types = cols(date = col_date(format = "%Y%m%d"),hour = col_time(format = "%H")))
    
    m=1
    so2_hour<-data.frame()
    no2_hour<-data.frame()
    ozone_hour<-data.frame()
    co_hour<-data.frame()
    
    for(j in 1:24) {
      so2_hour[j,1:38]<-temp1[m,1:38]
      no2_hour[j, 1:38]<-temp1[m+2, 1:38]
      ozone_hour[j, 1:38]<-temp1[m+4, 1:38]
      co_hour[j, 1:38]<-temp1[m+6, 1:38]
    
      m=j*8+1
    } 
    
    so2_all<-rbind(so2_all, so2_hour)
    no2_all<-rbind(no2_all, no2_hour)
    ozone_all<-rbind(ozone_all, ozone_hour)
    co_all<- rbind(co_all, co_hour)
    
  }
  
  
  so2[[year]]<-so2_all
  no2[[year]]<-no2_all
  ozone[[year]]<-ozone_all
  co[[year]]<-co_all
  names(so2[[year]])<-colnam
  names(no2[[year]])<-colnam
  names(ozone[[year]])<-colnam
  names(co[[year]])<-colnam

}


sta_2018<-list()
cor_2018<-list()

for (nam in sta_names){

  sta_2018[[nam]]<-cbind(pm2.5[["2018"]][,c("date", "hour",nam)], pm10[["2018"]][,nam],
                         so2[["2018"]][,nam], no2[["2018"]][,nam], 
                         ozone[["2018"]][,nam], co[["2018"]][,nam])  
  
  names(sta_2018[[nam]])<-c("date", "hour", "pm2.5", "pm10", "so2", "no2", "ozone", "co")
  
  
  cor_2018[[nam]]<-cor(sta_2018[[nam]][, -c(1,2)], use = "na.or.complete") 
  
  
}


cor.result.2018<-data.frame()

for (o in 1:35){
  
  
  cor.result.2018[o, 1:3]<-cor_2018[[o]][1,c(3,4,5)]
}

names(cor.result.2018)<-c("pm2.5_so2","pm2.5_no2","pm2.5_ozone")




sta_2019<-list()
cor_2019<-list()

for (nam in sta_names){
  
  sta_2019[[nam]]<-cbind(pm2.5[["2019"]][,c("date", "hour",nam)], pm10[["2019"]][,nam],
                         so2[["2019"]][,nam], no2[["2019"]][,nam], 
                         ozone[["2019"]][,nam], co[["2019"]][,nam])  
  
  names(sta_2019[[nam]])<-c("date", "hour", "pm2.5", "pm10", "so2", "no2", "ozone", "co")
  
  
  cor_2019[[nam]]<-cor(sta_2019[[nam]][, -c(1,2)], use = "na.or.complete") 
  
  
}


cor.result.2019<-data.frame()

for (o in 1:35){
  
  
  cor.result.2019[o, 1:3]<-cor_2019[[o]][1,c(3,4,5)]
}

names(cor.result.2019)<-c("pm2.5_so2","pm2.5_no2","pm2.5_ozone")




dongsi_2018<-sta_2018[["dongsi"]] 






