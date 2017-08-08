soh_estimate<-function(data_in,size=250,len=750,elen=200,...)
{
  #data_in should be a n*2 matrix, with first line the time, and second line the voltage 
  #parameters for training set
  #datapath<-"~/Data/"
  
  #read training data
  data_all<-read.csv(sprintf('Data/%d_predict.csv',size),header=T, sep=",")
  data_soh<-read.csv(sprintf("Data/%d_intotal.csv",size),header=T, sep=",")
  #################################
  ##need to normalize data_soh input
  #for(i in 1:dim(data_all)[1])
  #{
  #  data_all[i,3]<-(i-1)%/%200+1
  #}
  
  #######can be omited later
  #####################
  #data_in should be a n*2 matrix, with time and voltage
  esti_len<-dim(data_in)[1] #the length of the data_in
  
  data_soh[,5]<-data_soh[,4]/size
  
  #define a distance function
  distance<-matrix(rep(0,len*2),nrow=len)
  
  
  for(i in 0:(len-1))
  {
    x<-data_all[(1:elen),1]
    y<-data_all[(i*elen+1):((i+1)*elen),3]
    xyspline<-smooth.spline(x,y,cv=T)
    sum<-10000
    for(j in 0:(199-esti_len-1))
    {
      sum_temp<-0
      for(k in 1:esti_len)
      {
        sum_temp<-sum_temp+(predict(xyspline,j+k)$y-data_in[k,2])^2
      }
      if(sum_temp<sum) sum<-sum_temp
    }
    distance[i+1,1]<-sum
    distance[i+1,2]<-data_all[(i*elen+1),2]
  }
  distance_new<-distance[order(distance[,1]),]
  newindex<-distance_new[1:5,2] #距离最近的5个数据的平均值作为估计值
  data_esti<-mean(data_soh[newindex,5])
  paste(sprintf("SOH=%.2f",data_esti*100),"%",sep="")
}