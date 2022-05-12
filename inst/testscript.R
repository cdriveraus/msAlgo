




set.seed(1)

#use future package for testing concurrent handling of multiple requests
library(future)
plan(strategy = 'multisession',workers=8) #set to 'sequential' for non parallel.

#If using server set TRUE, otherwise just runs locally
opencpu <- FALSE
restrserve <- FALSE
trifork <- TRUE

require(data.table)
require(bigIRT)
library(ggplot2)
require(msAlgo)


# Simulate items and persons ----------------------------------------------
scaleNames = c('maths','english')
sc <- t(chol(matrix(c(2,1,1,.8),2,2,dimnames = list(scaleNames,NULL)))) #scale covariance cholesky factor

Npersons=10

#person covariates
pcovs=data.table(age=seq(-4,4,length.out=Npersons),
  ses=rnorm(Npersons,0,2),
  height=rnorm(Npersons,0,.2))

#simulate persons
persons=bigIRT:::simPersons(N=Npersons,
  mu=c(0,.3),
  # covs=pcovs,
  # beta=matrix(c(.3,.2,0, .4, .3, 0),byrow=TRUE,2,3)
  scaleChol=sc
)

#limited checks on generated persons
cor(persons)
cov2cor(sc)
plot(persons$age,persons$maths)


NperScale=1000 #number of items per scale

#item covariates
icovs=data.table(grade=seq(-4,4,length.out=NperScale),
  clarity=rnorm(NperScale,0,2),
  specificity=rnorm(NperScale,0,.2))

#generate items
items <- bigIRT:::simItems(NperScale = NperScale,scaleNames = colnames(sc),invspAmu = bigIRT:::inv_log1p_exp(c(1,1)),
  invspASD = c(1,1),Bmu = c(0,0),BSD = c(3,2),logitCmu = c(-10,-10),logitCSD = c(0,0)
  # covs = icovs,
  # invspAbeta = matrix(0,byrow=TRUE,2,3),
  # Bbeta = matrix(c(1,0,.1, 0,-.2,0),byrow=TRUE,2,3),
  # logitCbeta = matrix(c(.1,0,.1, 0,-.2,0),byrow=TRUE,2,3)
)

items$C <- 0 #for 2pl model

items$Item <- paste0('i_',items$Scale,'_',1:nrow(items)) #name items

#limited testing of item generation
itemmat=as.matrix(items[,c('A','B','C',colnames(icovs)),with=FALSE])
cor(itemmat)
cor(itemmat[1:NperScale,])
cor(itemmat[(NperScale+1):(NperScale*2),])
plot(icovs$clarity[1:NperScale],items$A[1:NperScale])




### simulation loops -- samples random person, selects item, progresses through assessment and repeats.

Nsims <-1 #number of (possibly concurrent) simulations to do
# Nassesments <- 20 #number of assessment sessions per sim
Nperassessment <- 40 #number of items per assessment session


## save items to msAlgo package when new item set desired.
# setwd("C:/Users/Driver/Seafile/mpib/msAlgo/")
# save('persons',file = './data/persons.rda')
# save(items,file = './data/items.rda')

if(restrserve || opencpu){
  rm(items) #ensure that we are using items saved to msAlgo package
  items <- msAlgo::items
}



# cito full run -----------------------------------------------------------

Nruns = 50

for(ri in 1:Nruns){
  setwd("C:/Users/Driver/Seafile/mpib/msAlgo/cito")
  fl=list.files()
  file.remove(fl[grep('\\d+\\.',fl)])

  setwd("C:/Users/Driver/Seafile/mpib/msAlgo")
  citoInput(items[Scale %in% unique(Scale)[1],],minlen = Nperassessment,maxlen = Nperassessment)
  setwd("C:/Users/Driver/Seafile/mpib/msAlgo/cito")
  system(command = paste0('CatAlg.exe'),wait = TRUE,ignore.stdout = FALSE)
  setwd("C:/Users/Driver/Seafile/mpib/msAlgo")

  ctheta <- data.frame(fread(file='./cito/1.theta.csv'))

  ctheta=ctheta[,c(1:(Nperassessment+4))[-c(1,Nperassessment+1)]] #skip missing cols
  colnames(ctheta) <- paste0(1:ncol(ctheta))
  ctheta$id <- 1:nrow(ctheta)
  colnames(ctheta)[Nperassessment+c(0,2)]=c('trueability','sd')
  sd(ctheta$trueability)
  ctheta <- melt.data.table(data.table(ctheta),id.vars = c('id','trueability','sd'),variable.name = 'Nitems',value.name = 'Ability')

  ctheta$Nitems<-as.integer(ctheta$Nitems)
  ctheta[,id:=factor(id)]

  # ctheta$Nitems[ctheta$Nitems %in% (Nperassessment+1)] <- NA#what is this? ctheta$variable[ctheta$variable %in% (Nperassessment+1)]-1
  ctheta<-ctheta[!is.na(Ability),]

  ctheta$SimID <- paste0('Sim',ri,'_',ctheta$id)
  if(ri==1) dcito <- ctheta else dcito <- rbind(dcito,ctheta)
}

#output to disk
save(dcito,file="C:/Users/Driver/Seafile/mpib/msAlgo/cito/dcito.rda")
save(items,file="C:/Users/Driver/Seafile/mpib/msAlgo/cito/items.rda")


ggplot(data = dcito[as.numeric(id) < 2,],mapping = aes(y=Ability,x=Nitems,colour=SimID,fill=SimID))+
  geom_line()+
  geom_hline(aes(yintercept=trueability,colour=SimID),size=1,alpha=.5,linetype=2)+
  geom_ribbon(mapping = aes(ymin=Ability-1.96*sd,ymax=Ability+1.96*sd),linetype=3,alpha=.05)+
  theme_bw()


#coverage - seems pretty bad, why?
dcito[,coverage:= sum(Ability > (trueability - 1.96* sd) & Ability < (trueability + 1.96* sd)) / (.N),by=Nitems]
plot(dcito[SimID %in% SimID[1],coverage],ylab='Coverage',xlab='Nitems')

#rmse
dcito[,rmse:=sqrt(mean((Ability-trueability)^2)),by=Nitems]
plot(dcito[SimID %in% SimID[1],rmse],ylab='rmse',xlab='Nitems')


#get person data from cito
persons = persons[1:length(unique(dcito$SimID)),]
persons$maths=dcito[!duplicated(SimID),trueability]





### end cito

# prototype algo ----------------------------------------------------------

truepersons <- copy(persons) #store true values for later
persons[,(scaleNames):= 0] #set initial ability to zero (placeholder)

starttime <- Sys.time()

simAssess <- function(ai){
  person <- ai#sample(1:nrow(persons),1)
  scale <- scaleNames[1]
  adat <- data.table(Item='xxxxxxxxx') #blank placeholder

  for(i in 1:Nperassessment){ #for every item in the assessment
    #select an item to present
    itemselectstarttime=Sys.time() #time this request

    if(opencpu) itemcode=c(jsonlite::fromJSON(system(intern = TRUE,ignore.stderr = TRUE,
      paste0(
        'curl http://localhost/ocpu/library/msAlgo/R/selectItem/json -d "items=items&scalename=',
        '\'',paste0(scale),'\'',
        '&ability=',unlist(persons[person,scale,with=FALSE]),'"')
    )))

    if(restrserve) itemcode=c(jsonlite::fromJSON(system(intern = TRUE,ignore.stderr = TRUE,
      paste0(
        'curl \'http://localhost:8080/selectItem_rserve?scalename=',
        '\'',paste0(scale),'\'',
        '&ability=',unlist(persons[person,scale,with=FALSE]),'\''))
    ))


    if(!opencpu & !restrserve) itemcode <- msAlgo:::selectItem(items=items,scalename =scale,
      ability=unlist(persons[person,scale,with=FALSE]))
    message(itemcode)
    print(Sys.time()-itemselectstarttime)


    #collect assessment data for this specific test item
    rowdat <- cbind(data.table(
      id=person,
      AssessmentID=paste0('sim',simi,'_',ai),
      trueability=unlist(truepersons[person,scale,with=FALSE]),
      AssessmentItemCount=i,
      item=itemcode,
      score=as.integer(NA),
      ability=0.0,
      abilitySD=0,
      items[Item %in% itemcode,]),
      pcovs[person,])

    if(i==1) adat <- rowdat else adat <- rbind(adat,rowdat) #combine assessment data

    #simulate response from student
    adat[i,score:= bigIRT:::simResponse(items[Item %in% itemcode,],unlist(truepersons[person,scale,with=FALSE])) ]

    #update ability estimate and get standard error
    startabilitytime=Sys.time()

    if(opencpu){
      fitarg=jsonlite::toJSON(adat)
      fitarg <- gsub('"','\\"',fitarg,fixed=TRUE)
      Ability=c(jsonlite::fromJSON(
        system(intern = TRUE,ignore.stderr = TRUE,
          paste0('curl http://localhost/ocpu/library/msAlgo/R/msFit/json -d "text=F&jsontextdat=\'',
            fitarg,'\'"')
        )
      ))
    }

    if(restrserve){
      fitarg=jsonlite::toJSON(adat)
      fitarg <- gsub('"','\\"',fitarg,fixed=TRUE)
      Ability=c(jsonlite::fromJSON(
        system(intern = TRUE,ignore.stderr = TRUE,
          paste0('curl -g \'localhost:8080/abilityEst_rserve?jsontextdat=\'',
            fitarg,'\'\'')
        )
      ))
    }


    if(!opencpu & !restrserve) Ability <- msAlgo:::msFit(jsonlite::toJSON(adat),
      AbilitySD = 2.75*sd(truepersons$maths)) #2.75 is good comparison to wle / cito

    print(Sys.time()-startabilitytime)


    persons[person,(scale):=Ability[1]]
    adat[nrow(adat),ability:=Ability[1]]
    adat[nrow(adat),abilitySD:=Ability[2]]


  }

  # if(ai==1){
  #   record <- data.frame(adat)
  #   record[(Nperassessment+1):(Nperassessment*nrow(persons)),] <- NA
  #   record <- data.table(record)
  # }
  # else record[((ai-1)*Nperassessment+1):(ai*Nperassessment),] <- adat

  return(adat)
}


record <- list() #placeholder for saved output

#simulation loop
for(simi in 1:Nsims){ #for every sim
  # record[[simi]] <- future({ #start calculating the following loop
  for(ai in 1:nrow(persons)){
    record[[ai+nrow(persons)*(simi-1)]] <-future( simAssess(ai))
  }
}

record2 <- value(record) #wait for simulation to finish then put output in record2

endtime <- Sys.time()
print(endtime-starttime)



# prototype plots ---------------------------------------------------------


record <- do.call(rbind,record2)
print(Sys.time()-a)

recordsml <- record[1:(4*Nperassessment),]
recordsml[,simID:=interaction(factor(AssessmentID),factor(id))]

require(ggplot2)
ggplot(recordsml,aes(y=ability, x=AssessmentItemCount, fill=simID,colour=simID))+
  geom_line(size=1)+
  theme_bw()+
  geom_hline(aes(yintercept=trueability,colour=simID),size=1,alpha=.5,linetype=2)+
  geom_ribbon(mapping = aes(ymin=ability-1.96*abilitySD,ymax=ability+1.96*abilitySD),linetype=3,alpha=.05)


sd(record[AssessmentItemCount %in% 1, trueability])

#sd plots
rec=record[AssessmentItemCount==max(AssessmentItemCount),]
plot(rec$trueability,rec$abilitySD)

#coverage
record[,coverage:= sum(ability > (trueability - 1.96* abilitySD) & ability < (trueability + 1.96* abilitySD)) / (.N),by=AssessmentItemCount]
plot(record$coverage[record$AssessmentID %in% record$AssessmentID[1]],ylab='Coverage',xlab='Nitems')

#rmse
record1=copy(record)
record1=record1[Scale %in% 'maths',]
record1[,rmse:=sqrt(mean((ability-trueability)^2)),by=AssessmentItemCount]
plot(record1[AssessmentID %in% AssessmentID[1],rmse],ylab='rmse',xlab='Nitems',ylim=c(0,max(c(record1$rmse,dcito$rmse))))
points(dcito[SimID %in% SimID[1],rmse],ylab='rmse',xlab='Nitems',col=2)
#
# #mse
# record[,Random:=ifelse(AssessmentID > (Nassesments/2),TRUE,FALSE)]
# record[,RMSE:=sqrt(mean((trueability-ability)^2)),by=interaction(Random,AssessmentItemCount)]
#
# ggplot(record,aes(y=RMSE,colour=Random,x=AssessmentItemCount))+geom_line()+theme_bw()

# end test ----------------------------------------------------------------


