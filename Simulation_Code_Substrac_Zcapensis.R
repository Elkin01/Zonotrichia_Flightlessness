Table=read.delim('C:/Users/Sebastian Avellaneda/Desktop/Último semestre 2/Trabajo de grado 2/Submit/Data_Zonotrichia.txt', header=T, dec= ",") #change the ubication

####Example with Tarsus

table=Table[,c('Location','Tarsus')] #change name of the trait

table=na.omit(table)

deltaramdvalues=c()

for (i in 1:1000)
  
{
  
  mLow=sample(table$Tarsus, 10, replace = FALSE) #change name of the trait
  mLow=data.frame(Loc=rep('Low', 10), trait=mLow)
  mLow=mean(mLow$trait)
  mLow=data.frame(Loc=rep('Low', 1), trait=mLow)
  
  mHigh=sample(table$Tarsus, 10, replace = FALSE) #change name of the trait
  mHigh=data.frame(Loc=rep('High', 10), trait=mHigh)
  mHigh=mean(mHigh$trait)
  mHigh=data.frame(Loc=rep('High', 1), trait=mHigh)
  
  substest=(mLow$trait - mHigh$trait)
  
  
  deltaramdvalues=c(deltaramdvalues, substest[[1]])
  
}

High <- table[1:10,2]
High=data.frame(Loc=rep('High', 10), trait=High)
High=mean(High$trait)

Low <-table[11:20,2]
Low=data.frame(Loc=rep('Low', 10), trait=Low)
Low=mean(Low$trait)

actualobserved=(Low - High)

actualobserved

length(which(deltaramdvalues<=actualobserved))/1000 ##P value final

#substract 1 to calculate p-value to heart, lungs and tarsus

#####plot

hist(deltaramdvalues, main='Simulation Tarsus value') #change name of the trait

abline(v=quantile(deltaramdvalues, 0.05), col='red')

abline(v=actualobserved, col='blue')
