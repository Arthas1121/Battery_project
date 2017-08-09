soh_estimate<-function(data_in,size=65,len=750,elen=200,...)
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
  
  #data_soh[,5]<-data_soh[,4]/size
  
  #define a distance function
  distance<-matrix(rep(0,len*2),nrow=len)
  lengthall<-dim(data_all)[1]
  m<-1
  
  timestart<-Sys.time()
  for(i in 0:(len-1))
  {
    #x<-data_all[(1:elen),1]
    #y<-data_all[(i*elen+1):((i+1)*elen),3]
    #xyspline<-smooth.spline(x,y,cv=T)
    ###############
    x<-0
    y<-0
    while(data_all[m,1]==i+1 && data_all<=lengthall)
    {
      x<-c(x,data_all[m,2])
      y<-c(y,data_all[m,3])
      m<-m+1
    }
    x<-x[-1]
    y<-y[-1]
    xyspline<-smooth.spline(x,y,cv=T)
    
    
    ##############
    sum<-10000
    for(j in 0:(length(x)-esti_len-1))
    {
      sum_temp<-0
      for(k in 2:esti_len)
      {
        sum_temp<-sum_temp+(predict(xyspline,data_in[k,1]+20*j)$y-data_in[k,2])^2*(data_in[k,1]-data_in[k-1,1])
      }
      if(sum_temp<sum) sum<-sum_temp
    }
    distance[i+1,1]<-sum
    distance[i+1,2]<-data_all[m-1,1]
  }
  timeend<-Sys.time()
  distance_new<-distance[order(distance[,1]),]
  newindex<-distance_new[1:5,2] #距离最近的5个数据的平均值作为估计值
  data_esti<-mean(data_soh[newindex,5])
  paste(sprintf("SOH=%.2f",data_esti*100),"%",sep="")
}