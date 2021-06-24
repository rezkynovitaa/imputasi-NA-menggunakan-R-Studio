dirtydata<-read.table(file.choose(),header = T,sep=",",stringsAsFactors = F) #memilih dirty data
View(dirtydata)
#string as factor berarti agar data tidak dibaca faktor
is.na.data.frame(dirtydata)
complete.cases(dirtydata)
#na.omit untuk mengeluarkan observasi yang mengandung nilai NA
dirtycomplete<-na.omit(dirtydata)
dirtycomplete
summary(dirtycomplete)
data<-length(dirtycomplete$Sepal.Length)
data
percent<-paste0(formatC((data/length(dirtydata$Sepal.Length)*100),format="fg",digits=2),"%")
percent
a<-(data/150)*100
persen<-paste(a,"%",sep=" ")
persen
dirtycomplete
for(i in 1:length(dirtycomplete)){
  dirtycomplete[,i][is.infinite(dirtycomplete[,i])]<-NA
}
dirtycomplete


#2
#mean
dirtydata<-read.table(file.choose(),header = T,sep=",",stringsAsFactors = F) #memilih dirty data
View(dirtydata)
#string as factor berarti agar data tidak dibaca faktor
dirtydata
dirtydataa<-dirtydata
for(i in 1:length(dirtydataa)){
  dirtydataa[,i][is.infinite(dirtydataa[,i])]<-NA
}
dirtydataa

#NA = Mean
for(i in 1:(length(dirtydataa)-1)){
  dirtydataa[,i][is.na(dirtydataa[,i])]<-mean(dirtydataa[,i],na.rm=T)
}
dirtydataa

#NA = median
for(i in 1:(length(dirtydataa)-1)){
  dirtydataa[,i][is.na(dirtydataa[,i])]<-median(dirtydataa[,i],na.rm=T)
}
dirtydataa

#NA = y duga (regresi)
y=dirtydataa$Sepal.Length
x1=dirtydataa$Sepal.Width
x2=dirtydataa$Petal.Width
modelreg<-lm(y~x1+x2,dirtydataa)
modelreg
I<-is.na(dirtydataa$Sepal.Length)
dirtydataa$Sepal.Length[I]<-predict(modelreg)
dirtydataa

#rasio
dirtydata<-read.table(file.choose(),header = T,sep=",",stringsAsFactors = F) #memilih dirty data
View(dirtydata)
dirtydataa<-dirtydata
for(i in 1:length(dirtydataa)){
  dirtydataa[,i][is.infinite(dirtydataa[,i])]<-NA
}
dirtydataa
y=dirtydataa$Sepal.Length
x1=dirtydataa$Sepal.Width
modelreg<-lm(y~x1,dirtydataa)
modelreg
I<-is.na(dirtydataa$Sepal.Length)
dirtydataa$Sepal.Length[I]<-predict(modelreg)
dirtydataa
II<-is.na(dirtydataa$Sepal.Width)
a<-sum(x1[!II])
a
b<-sum(dirtydataa$Sepal.Length[!II])
b
rasio<-a/b
rasio

III<-is.na(dirtydataa$Sepal.Width)
aa<-sum(dirtydataa$Sepal.Width[!III])
bb<-sum(dirtydataa$Sepal.Length[!III])
r<-aa/bb
r