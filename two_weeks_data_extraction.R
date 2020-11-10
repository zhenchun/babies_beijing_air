library(readr) #加载读取excel包
library(stringr)
library(lubridate)

pollutants<-c("NO2", "PM2.5", "PM10", "CO", "ozone", "SO2")

visit_date <- read_csv("C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/visit_date.csv")

result<-data.frame()

result[1:861,1]<-visit_date$id
result[1:861,2]<-visit_date$date

b<-read_csv(paste0("C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/babies_pred_NO2.csv"))
b<-b[,-1]

coln<-c()
uniq<-unique(substr(colnames(b),1, 5))



for(j in 1: length(uniq)){
  
  vit<-c()
  
  for (i in 1:6) {
    
    vit<-c(vit, str_c(uniq[j],"Q",i))
  }
  
  coln<-c(coln, vit)
}



for (p in pollutants){

dat<- read_csv(paste0("C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/babies_pred_",p,".csv")) #a是导入的数据表新命名名称
a<-dat[,-1] #删除第一列序号，不用重复
attach(a)
bb2=data.frame() #创建bb2空的数据集件

bb1=data.frame() #创建一个空的bb1数据集
collast<-as.numeric(str_sub(colnames(a),7,7)) #末位数字提取，根据末位数字判断复制次数
collast3<-as.numeric(str_sub(colnames(a),3,5)) #中间3位数字提取
#复制第一列
   if(collast3[1]==1){
       if(collast3[1]==collast3[2]){
       bb1<-data.frame(rep(a[1],each=collast[2]-collast[1]))
       bb2<-data.frame(append(bb2,bb1))
     }
   else{
       bb1<-data.frame(rep(a[1],each=6))
       bb2<-data.frame(append(bb2,bb1))
      }
    }
bb1=data.frame()
#复制第二列到最后一列
for(i in 2:length(colnames(a))) { #对已有列进行遍历
  if(!is.na(collast[i+1])){ #重复除最后一列数据外的所有列
    if(collast3[i]==collast3[i+1]){
      bb1<-data.frame(rep(a[i],each=collast[i+1]-collast[i]))
      bb2<-data.frame(append(bb2,bb1))
    }
    else{
      if(collast3[i]==collast3[i-1]){
        bb1<-data.frame(rep(a[i],each=7-collast[i])) 
        bb2<-data.frame(append(bb2,bb1))} #写入新的数据集bb2
      else if(collast3[i]!=collast3[i-1]){
        bb1<-data.frame(rep(a[i],each=6)) 
        bb2<-data.frame(append(bb2,bb1))} #写入新的数据集bb2
    }
  }
  else{ #重复最后一列数据
    if(collast3[i]!=collast3[i-1]){
      bb1<-data.frame(rep(a[i],each=6))
      bb2<-data.frame(append(bb2,bb1))
    }
    else{
      bb1<-data.frame(rep(a[i],each=7-collast[i]))
      bb2<-data.frame(append(bb2,bb1))
    }
  }
  bb1=data.frame()
} 



colnames(bb2)<-coln

total_col=cbind(dat[,1],bb2) 


for(i in 1:861){
  
  result[i,p]= mean(total_col[between(total_col$X1, result[i, 2]-14, result[i, 2]), 
                              colnames(total_col)==result[i,1]])
}

}












