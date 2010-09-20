########################
#### Function Inputs
#### sf1/sf3
#### directory (default to wd)
#### demographic variable or variables desired
#### level
#### 
########################



demographics.add<-function(dem=NULL,state=NULL,statefips=FALSE,level=c("tract","blk","blkgrp","cdp"),census="sf1"){

demographics.add.aux<-function(dem=NULL,state=NULL,statefips=FALSE,census="sf1",directory=NULL){
	require(UScensus2000)
	data("countyfips",envir = parent.frame())
	assign("temp",countyfips)
	assign("countyfips",temp)
#########Check and see how they input the stat

state<-check.state(state,statefips)

if(is.null(state)){
	stop("Not a State! \n")
	}



#########Check and see how they input the stat	

######### Load other needed things
	data("states.names",envir = parent.frame())
	assign("temp",states.names)
	assign("states.names",temp)

	data("states.names.cap",envir = parent.frame())
	assign("temp",states.names.cap)
	assign("states.names.cap",temp)
	
	data("state.ab",envir = parent.frame())
	#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/state.ab.rda")
	assign("temp",state.ab)
	assign("state.ab",temp)
	
	data("census.demographics.list",envir = parent.frame())
	assign("temp",census.demographics.list)
	assign("census.demographics.list",temp)
	
	
	#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/census.demographics.list.rda")
#	assign("temp",census.demographics.list)
#	rm(census.demographics.list,envir = .GlobalEnv)
#	assign("census.demographics.list",temp)
######### Load other needed things
	


decider.fun<-function(state=NULL,dem=NULL,cdl=census.demographics.list,type=c(1,2,3)){
	index.state<-which(states.names%in%state)
	index.file<-vector()

	for(i in 1:length(cdl)){
		if(length(which(cdl[[i]]%in%dem))>0){
			index.file<-c(index.file,i)
			}
		}

temp1<-(max(index.file)>=10 & min(index.file)>=10)
temp2<-(max(index.file)<10)
temp3<- (temp1==FALSE & temp2==FALSE)
type<-type[c(temp1,temp2,temp3)]

switch.fun<-function(type){
switch(type, 
index.out <- paste("000",index.file,sep=""), 
index.out <- paste("0000",index.file,sep=""),
index.out <- c(paste("0000",index.file[index.file<10],sep=""),paste("000",index.file[index.file>=10],sep=""))
)
}
index.out<-switch.fun(type)
out<-list(statei=index.state,filei=index.out,index.raw=index.file)	
}
####### Generate index list

il<-decider.fun(state=state,dem=dem)

####### Generate index list

###########Grab Geofile
require(XML)
url<-paste("http://ftp.census.gov/census_2000/datasets/Summary_File_1/",states.names.cap[il$statei],"/",state.ab[il$statei],"geo_uf1.zip",sep="")
fileName<-paste(state.ab[il$statei],"000","geo_uf1.zip",sep="")
if(!file.exists(paste(tempdir(),"/",states.names[il$statei],"",sep=""))){dir.create(paste(tempdir(),"/",states.names[il$statei],"/",sep=""),recursive=TRUE)}
download.file(url,destfile=paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""))
unzip(zipfile=paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""),exdir=paste(tempdir(),"/",states.names[il$statei],"",sep=""))
#file.remove(paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""))
###########Grab Geofile


#########################################
###### Need a switcher function built on top of this
#########################################

###################
###########Grab file i
for(i in 1:length(il$filei)){
url<-paste("http://ftp.census.gov/census_2000/datasets/Summary_File_1/",states.names.cap[il$statei],"/",state.ab[il$statei],il$filei[i],"_uf1.zip",sep="")
fileName<-paste(state.ab[il$statei],il$filei[i],"_uf1.zip",sep="")
if(!file.exists(paste(tempdir(),states.names[il$statei],"",sep=""))){dir.create(paste(tempdir(),states.names[il$statei],"/",sep=""),recursive=TRUE)}
download.file(url,destfile=paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""))
unzip(zipfile=paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""),exdir=paste(tempdir(),"/",states.names[il$statei],"",sep=""))
#file.remove(paste(tempdir(),"/",states.names[il$statei],"/",fileName,sep=""))
}
###########Grab file i
###################


geo.file.build<-function(si){
	###########################################################################
####BUILD GEO FILE
###########################################################################
geo<-readLines(paste(tempdir(),"/",states.names[si],"/",state.ab[si],"geo.uf1",sep=""))
fileName<-paste(state.ab[si],"geo.uf1",sep="")
#file.remove(paste(tempdir(),"/",states.names[si],"/",fileName,sep=""))
######### Record Codes
fileid<-substr(geo,1,6)
stusab<-substr(geo,7,8)
sumlev<-substr(geo,9,11)
geocomp<-substr(geo,12,13)
chariter<-substr(geo,14,16)
cifsn<-substr(geo,17,18)
logrecno<-substr(geo,19,25)
######### Record Codes

######### Geographic Area Codes
region<-substr(geo,26,26)
division<-substr(geo,27,27)
statec<-substr(geo,28,29)
state<-substr(geo,30,31)
county<-substr(geo,32,34)
countysc<-substr(geo,35,36)
cousub<-substr(geo,37,41)
cousubcc<-substr(geo,42,43)
cousubsc<-substr(geo,44,45)
place<-substr(geo,46,50)
placecc<-substr(geo,51,52)
placedc<-substr(geo,53,53)
placesc<-substr(geo,54,55)
tract<-substr(geo,56,61)
blkgrp<-substr(geo,62,62)
block<-substr(geo,63,66)
######### Geographic Area Codes


######### Area Characteristics
pop100<-substr(geo,293,301)
hu100<-substr(geo,302,310)
latitude<-substr(geo,311,319)
longitude<-substr(geo,320,329)


######### Area Characteristics


######### Build geo ident file
geo.id.file<-cbind(fileid,stusab,sumlev,geocomp,chariter,cifsn,logrecno,region,division,statec,state,county,countysc,cousub,cousubcc,cousubsc,place,placecc,placedc,placesc,tract,blkgrp,block,pop100,hu100,latitude,longitude)
######### Build geo ident file
###########################################################################
####BUILD GEO FILE
###########################################################################
	}

########Load and save geofile
geo.id.file<-geo.file.build(il$statei)
########Load and save geofile

########Load and save otherfiles
demographic.files.list<-vector("list",length(il$filei))
temp.index<-unique(il$index.raw)
for(i in 1:length(il$filei)){
	demographic.files.list[[i]]<-read.csv(file=paste(tempdir(),"/",states.names[il$statei],"/",state.ab[il$statei],il$filei[i],".uf1",sep=""),header=FALSE,colClasses="character")
	
	names(demographic.files.list[[i]])<-census.demographics.list[[temp.index[i]]]
	
	#file.remove(paste(tempdir(),"/",states.names[il$statei],"/",state.ab[il$statei],il$filei[i],".uf1",sep=""))
	}
#file.remove(tempdir(),"/",states.names[il$statei])
out<-list(geo.file=geo.id.file,demog=demographic.files.list,state=state,dmi=il$index.raw)
}



matchDemog.tract<-function(input,dem){
##Load
#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/census.demographics.list.rda")
data(census.demographics.list,envir = parent.frame())
	assign("temp",census.demographics.list)
	assign("census.demographics.list",temp)
##Load

geo.id.file<-input$geo.file
state<-input$state
require(UScensus2000tract)
data(list=paste(state,".tract",sep=""),envir = parent.frame())
assign("temp.tract",get(paste(state,".tract",sep="")))
#rm(list=paste(state,".tract",sep=""))




fips<-as.character(temp.tract$tract)
fips[which(nchar(fips)==4)]<-paste(fips[which(nchar(fips)==4)],"00",sep="")
fips<-paste(temp.tract$county,fips,sep="")

geo.id.file<-geo.id.file[which(geo.id.file[,"sumlev"]%in%"140"==TRUE),]

m<-match(fips,paste(geo.id.file[,"county"],geo.id.file[,"tract"],sep=""))

#cat("fips:",length(fips),"vs"," tract",length(which(is.na(m)==FALSE)),"\n")

#test<-geo.id.file[m,c("county","tract","pop100")]
#test[which(test[,"tract"]=="950100"),]

temp<-as.matrix(temp.tract@data)

m1<-vector("list",length(input$demog))

for(i in 1:length(input$demog)){
	m1[[i]]<-match(geo.id.file[m,"logrecno"],input$demog[[i]][,"LOGRECNO"])
	}



temp2<-vector(length=nrow(temp))

for(i in 1:length(m1)){
temp2<-cbind(temp2,input$demog[[i]][m1[[i]],dem[dem%in%census.demographics.list[[input$dmi[i]]]]])
}
temp2<-temp2[,2:ncol(temp2)]
colnames(temp2)<-dem

temp<-data.frame(cbind(temp,temp2),stringsAsFactors=FALSE)
slot(temp.tract,"data")<-temp

temp.tract
}

matchDemog.blk<-function(input,dem){
##Load
#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/census.demographics.list.rda")
data("census.demographics.list",envir = parent.frame())
	assign("temp",census.demographics.list)
	assign("census.demographics.list",temp)
##Load

geo.id.file<-input$geo.file
state<-input$state
require(UScensus2000blk)
data(list=paste(state,".blk",sep=""),envir = parent.frame())
assign("temp.blk",get(paste(state,".blk",sep="")))
#rm(list=paste(state,".blk",sep=""))




fips<-temp.blk$fips


m<-match(fips,paste(geo.id.file[,"county"],geo.id.file[,"tract"],geo.id.file[,"block"],sep=""))

#cat("fips:",length(fips),"vs"," blk",length(which(is.na(m)==FALSE)),"\n")

#test<-geo.id.file[m,c("county","blk","pop100")]
#test[which(test[,"blk"]=="950100"),]

temp<-as.matrix(temp.blk@data)

m1<-vector("list",length(input$demog))

for(i in 1:length(input$demog)){
	m1[[i]]<-match(geo.id.file[m,"logrecno"],input$demog[[i]][,"LOGRECNO"])
	}




temp2<-vector(length=nrow(temp))

for(i in 1:length(m1)){
temp2<-cbind(temp2,input$demog[[i]][m1[[i]],dem[dem%in%census.demographics.list[[input$dmi[i]]]]])
}
temp2<-temp2[,2:ncol(temp2)]
colnames(temp2)<-dem

temp<-data.frame(cbind(temp,temp2),stringsAsFactors=FALSE)
slot(temp.blk,"data")<-temp

temp.blk
}

matchDemog.blkgrp<-function(input,dem){
##Load
#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/census.demographics.list.rda")
data("census.demographics.list",envir = parent.frame())
	assign("temp",census.demographics.list)
	assign("census.demographics.list",temp)
##Load

geo.id.file<-input$geo.file
state<-input$state
require(UScensus2000blkgrp)
data(list=paste(state,".blkgrp",sep=""),envir = parent.frame())
assign("temp.blkgrp",get(paste(state,".blkgrp",sep="")))
#rm(list=paste(state,".blkgrp",sep=""))




fips<-as.character(temp.blkgrp$tract)
fips[which(nchar(fips)==4)]<-paste(fips[which(nchar(fips)==4)],"00",sep="")
fips<-paste(as.character(temp.blkgrp$county),fips,as.character(temp.blkgrp$blkgrp),sep="")

geo.id.file<-geo.id.file[which(geo.id.file[,"sumlev"]%in%"150"==TRUE),]

m<-match(fips,paste(geo.id.file[,"county"],geo.id.file[,"tract"],geo.id.file[,"blkgrp"],sep=""))

#cat("fips:",length(fips),"vs"," tract",length(which(is.na(m)==FALSE)),"\n")

#test<-geo.id.file[m,c("county","tract","pop100")]
#test[which(test[,"tract"]=="950100"),]

temp<-as.matrix(temp.blkgrp@data)

m1<-vector("list",length(input$demog))

for(i in 1:length(input$demog)){
	m1[[i]]<-match(geo.id.file[m,"logrecno"],input$demog[[i]][,"LOGRECNO"])
	}




temp2<-vector(length=nrow(temp))

for(i in 1:length(m1)){
temp2<-cbind(temp2,input$demog[[i]][m1[[i]],dem[dem%in%census.demographics.list[[input$dmi[i]]]]])
}
temp2<-temp2[,2:ncol(temp2)]
colnames(temp2)<-dem

temp<-data.frame(cbind(temp,temp2),stringsAsFactors=FALSE)
slot(temp.blkgrp,"data")<-temp

temp.blkgrp
}

matchDemog.cdp<-function(input,dem){
##Load
#load("/Users/zack/Desktop/Census R Packages/Census.Packages/census2000add/data/census.demographics.list.rda")
data("census.demographics.list",envir = parent.frame())
	assign("temp",census.demographics.list)
	assign("census.demographics.list",temp)
##Load

geo.id.file<-input$geo.file
state<-input$state
require(UScensus2000cdp)
data(list=paste(state,".cdp",sep=""),envir = parent.frame())
assign("temp.cdp",get(paste(state,".cdp",sep="")))
#rm(list=paste(state,".cdp",sep=""))



if(state=="district_of_columbia"){
	
	fips<-temp.cdp$place
	geo.id.file<-geo.id.file[which(geo.id.file[,"sumlev"]%in%"160"==TRUE),]
	m<-match(fips,geo.id.file["place"])
	temp<-as.matrix(temp.cdp@data)
	m1<-vector("list",length(input$demog))
for(i in 1:length(input$demog)){
	m1[[i]]<-match(geo.id.file["logrecno"],input$demog[[i]][,"LOGRECNO"])
	}
temp2<-vector()
for(i in 1:length(m1)){
	temp2<-c(temp2,input$demog[[i]][m1[[i]],dem[dem%in%census.demographics.list[[input$dmi[i]]]]])
}
temp3<-matrix(c(temp,temp2),nc=(length(temp)+length(dem)),nr=1)
colnames(temp3)<-c(colnames(temp),dem)
temp<-data.frame(temp3,stringsAsFactors=FALSE)
slot(temp.cdp,"data")<-temp	
	}else{
fips<-temp.cdp$place
geo.id.file<-geo.id.file[which(geo.id.file[,"sumlev"]%in%"160"==TRUE),]
m<-match(fips,geo.id.file[,"place"])
temp<-as.matrix(temp.cdp@data)
m1<-vector("list",length(input$demog))
for(i in 1:length(input$demog)){
	m1[[i]]<-match(geo.id.file[m,"logrecno"],input$demog[[i]][,"LOGRECNO"])
	}
temp2<-vector(length=nrow(temp))

for(i in 1:length(m1)){
temp2<-cbind(temp2,input$demog[[i]][m1[[i]],dem[dem%in%census.demographics.list[[input$dmi[i]]]]])
}
temp2<-temp2[,2:ncol(temp2)]
colnames(temp2)<-dem
temp<-data.frame(cbind(temp,temp2),stringsAsFactors=FALSE)
slot(temp.cdp,"data")<-temp
}


temp.cdp
}


demsTomatch<-demographics.add.aux(dem,state,statefips)


if(level=="tract"){
	out<-matchDemog.tract(input=demsTomatch,dem=dem)
	}else if(level=="blkgrp"){
	out<-matchDemog.blkgrp(demsTomatch,dem)
		
		}else if(level=="blk"){
		out<-matchDemog.blk(demsTomatch,dem)
			
			}else if(level=="cdp"){
				out<-matchDemog.cdp(demsTomatch,dem)
				}
out<-demToNum(out)
out
}



# test<-demographics.add(dem="P001001",state="ri",level="blkgrp")
# test<-demographics.add(dem=c("P001001","P045003"),state="ri",level="tract")














