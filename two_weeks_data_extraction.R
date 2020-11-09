library(readr) #加载读取excel包
library(stringr)
dat<- read_csv("C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/babies_pred_NO2.csv") #a是导入的数据表新命名名称
a<-dat[,-1] #删除第一列序号，不用重复
attach(a)
bb2=data.frame() #创建bb2空的数据集件
bb1=data.frame() #创建一个空的bb1数据集
collast<-as.numeric(str_sub(colnames(a),7,7)) #末位数字提取，根据末位数字判断复制次数
for(i in 1:length(colnames(a))) { #对已有列进行遍历
  if(!is.na(collast[i+1])){ #重复除最后一列数据外的所有列
    if(collast[i+1] == collast[i]) { #地址没有改变
      bb1<-data.frame(rep(a[i],each=6)) #重复6遍
      bb2<-data.frame(append(bb2,bb1))} #写入新的数据集bb2
    else if(collast[i+1] > collast[i]) { #地址发生改变
      bb1<-data.frame(rep(a[i],each=collast[i+1] - collast[i]))
      bb2<-data.frame(append(bb2,bb1))} 
    else if(collast[i+1] < collast[i]){
      bb1<-data.frame(rep(a[i],each=7-collast[i]))
      bb2<-data.frame(append(bb2,bb1))
    }
  }
  else{ #重复最后一列数据
    bb1<-data.frame(rep(a[i],each=7-collast[i]))
    bb2<-data.frame(append(bb2,bb1))
  }
  bb1=data.frame()
} 

coln<-data.frame()

for(i in 1:6){
  
  coln<-c(coln, str_c(unique(substr(colnames(a), 1,5)),"Q",i))
}

colnames(total_col)<-coln



detach(a)
write.csv(x=total_col,file ="C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/bb_2w_total.csv") #导出为csv文件


colnames(total_col)<-coln



visit_date <- read_excel("C:/Users/zy125/Box Sync/Postdoc/BABIES/exposure_data_v2/visit_date.xlsx", col_types = c("text", "date"))

total_col=cbind(dat[,1],bb2) 

result<-data.frame()

result[1:861,1]<-visit_date$id
result[1:861,2]<-visit_date$date

for(i in 1:861){

    result[i,3]<-mean(total_col[between(total_col$X1, result[i, 2], visit_date[i, 2]+14), 
                            colnames(total_col)==result[i,1]], use=complete.cases())
      }
