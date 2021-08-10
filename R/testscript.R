if(FALSE){


# Simulate items and persons ----------------------------------------------

  set.seed(1)

  #use future package for testing concurrent handling of multiple requests
  library(future)
  plan(strategy = 'sequential',workers=4)

  #If using opencpu server, otherwise just run locally
  opencpu <- FALSE
  restrserve=TRUE

  require(data.table)
  require(bigIRT)
  require(msAlgo)

  scaleNames = c('maths','english')
  sc <- t(chol(matrix(c(2,1,1,.8),2,2,dimnames = list(scaleNames,NULL)))) #scale covariance cholesky factor

  Npersons=100

  #person covariates
  pcovs=data.table(age=seq(-4,4,length.out=Npersons),
    ses=rnorm(Npersons,0,2),
    height=rnorm(Npersons,0,.2))

  #simulate persons
  persons=bigIRT:::simPersons(N=Npersons,
    mu=c(0,.3),
    scaleChol=sc,
    covs=pcovs,
    beta=matrix(c(.3,.2,0, .4, .3, 0),byrow=TRUE,2,3)
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
  items <- bigIRT:::simItems(NperScale = 1000,scaleNames = colnames(sc),logAmu = bigIRT:::inv_log1p_exp(c(3,3)),
    logASD = c(0,.2),Bmu = c(0,0),BSD = c(3,4),logitCmu = c(-10,-10),logitCSD = c(0,0),
    covs = icovs,
    logAbeta = matrix(c(.01,.2,0, 0,-.2,0),byrow=TRUE,2,3),
    Bbeta = matrix(c(1,0,.1, 0,-.2,0),byrow=TRUE,2,3),
    logitCbeta = matrix(c(.1,0,.1, 0,-.2,0),byrow=TRUE,2,3)
  )

  items$C <- 0 #for 2pl model

  items$Item <- paste0('i_',items$Scale,'_',1:nrow(items)) #name items

  #limited testing of item generation
  itemmat=as.matrix(items[,c('A','B','C',colnames(icovs)),with=FALSE])
  cor(itemmat)
  cor(itemmat[1:NperScale,])
  cor(itemmat[(NperScale+1):(NperScale*2),])
  plot(icovs$clarity[1:NperScale],items$A[1:NperScale])


  #simulation loops -- samples random person, selects item, progresses through assessment and repeats.

  Nassesments <- 5 #number of assessment sessions
  Nperassessment <- 50 #number of items per assessment
  truepersons <- copy(persons)
  persons[,(scaleNames):= 0]

  ## save items to msAlgo package when new item set desired.
  # setwd("C:/Users/Driver/Seafile/mpib/msAlgo/")
  # save('persons',file = './data/persons.rda')
  # save(items,file = './data/items.rda')

  rm(items) #ensure that we are using items saved to msAlgo package
  items <- msAlgo::items

  Nsims <-4 #number of (concurrent) simulations to do
  starttime <- Sys.time()

  record <- list() #placeholder for saved output

  #simulation loop
  for(simi in 1:Nsims){ #for every sim
    record[[simi]] <- future({ #start calculating the following loop

      for(ai in 1:Nassesments){ #for every assessment, sample random person and scale to test
        person <- sample(1:nrow(persons),1)
        scale <- sample(scaleNames,1)
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
              'curl localhost:8080/selectItem_rserve?scalename=',
              '\'',paste0(scale),'\'',
              '&ability=',unlist(persons[person,scale,with=FALSE]))
          )))

          if(!opencpu & !restrserve) itemcode <- msAlgo:::selectItem(items=msAlgo::items,scalename =scale,
            ability=unlist(persons[person,scale,with=FALSE]))

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

          if(restrserve)

          if(!opencpu & !restrserve) Ability <- msAlgo:::msFit(jsonlite::toJSON(adat))

          print(Sys.time()-startabilitytime)


          persons[person,(scale):=Ability[1]]
          adat[nrow(adat),ability:=Ability[1]]
          adat[nrow(adat),abilitySD:=Ability[2]]


        }

        if(ai==1) record <- (adat) else record <- rbind(record,adat)

      }
      return(record)
    })
  }

  record2 <- value(record) #wait for simulation to finish then put output in record2

  endtime <- Sys.time()
  print(endtime-starttime)



  # plots -------------------------------------------------------------------



  record <- do.call(rbind,record2)
  print(Sys.time()-a)

  recordsml <- record[1:(4*Nperassessment),]
  recordsml[,simID:=interaction(factor(AssessmentID),factor(id))]

  require(ggplot2)
  ggplot(recordsml,aes(y=ability, x=AssessmentItemCount, fill=simID,colour=simID))+
    geom_line()+
    theme_bw()+
    geom_hline(aes(yintercept=trueability,colour=simID),size=1,alpha=.5,linetype=2)+
    geom_ribbon(mapping = aes(ymin=ability-1.96*abilitySD,ymax=ability+1.96*abilitySD),alpha=.1)

  #
  # #mse
  # record[,Random:=ifelse(AssessmentID > (Nassesments/2),TRUE,FALSE)]
  # record[,RMSE:=sqrt(mean((trueability-ability)^2)),by=interaction(Random,AssessmentItemCount)]
  #
  # ggplot(record,aes(y=RMSE,colour=Random,x=AssessmentItemCount))+geom_line()+theme_bw()

  # end test ----------------------------------------------------------------



}
