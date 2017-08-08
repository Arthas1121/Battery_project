soh_estimate<-function(data_in,size=250,len=800,elen=200,...)
{
  #parameters for training set
  #datapath<-"~/Data/"
  
  #read training data
  data_all<-read.csv(sprintf('Data/%d_predict.csv',size),header=T, sep=",")
  data_soh<-read.csv(sprintf("Data/%d_intotal.csv",size),header=T, sep=",")
  #################################
  ##need to normalize data_soh input
  #####################
  #data_in should be a n*2 matrix, with time and voltage
  esti_len<-dim(data_in)[1] #the length of the data_in
  
  data_soh[,5]<-data_soh[,4]/size
  
  #define a distance function
  distance<-matrix(rep(0,len*2),nrow=len)
  
  
  for(i in 1:len)
  {
    x<-data_all[(1:elen),1]
    y<-data_all[(i*elen+1):((i+1)*elen),4]
    xyspline<-smooth.spline(x,y,cv=T)
    sum<-10000
    for(j in 0:(len-esti_len-1))
    {
      sum_temp<-0
      for(k in 1:esti_len)
      {
        sum_temp<-sum_temp+(predict(xyspline,j+k)$y-esti[k,4])^2
      }
      if(sum_temp<sum) sum<-sum_temp
    }
    distance[i,1]<-sum
    distance[i,2]<-data_all[(i*elen+1),3]
  }
  distance_new<-distance[order(distance[,2]),]
  newindex<-distance_new[2:11,3] #距离最近的10个数据的平均值作为估计值
  data_esti<-mean(data_soh[newindex,5])
  print("SOH=%.2f",data_esti)
}