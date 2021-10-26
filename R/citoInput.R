#' Convert generated data in R to cito text files - needs further dev
#'
#' @param adat
#' @param nsegments
#' @param minlen
#' @param maxlen
#' @param pval
#' @param ntests
#' @param scale
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
  citoInput <- function(adat,nsegments=1,minlen=10,maxlen=minlen,pval=0.6,
    ntests=1, scale=1, folder=file.path(getwd(),'cito/seeding')){

    out <- list()
    out$category <- data.frame(key=1:nrow(adat),parent=0)

    out$item <- data.frame(key=1:nrow(adat),cluster= -1, seq = 0)

    ecdat <- c(-20,-1.5,-0.5,0.5,1.5,20,rep(0,26)) #not sure what these are for but they appear in cito scale text

    out$scale <- data.frame(key=1:length(unique(adat$Scale)), model=2, calib = 0,
      geomn=1,transA=1,transB=1,ecperscount=0, ecintcount=0)
    for(i in 1:length(ecdat)){
      out$scale <- cbind(out$scale,ecdat[i])
    }

    out$segment <- data.frame(key=1:nsegments, testid= 1,
      first=c(1,rep(0,nsegments-1)),
      lin=0, #0 is adaptive, 1 is linear
      minlen=minlen, maxlen=maxlen, #these refer to total test length not segment length, apparently
      startlen=1,#number of random item selections in segment
      startlo=-2, starthi=2,
      stopse=0, ecrate=1.0, #lower number = stricter exposure control, 1 is no exposure control, 0 is infinite.
      pval=pval, #target difficulty
      nextscore=0, #for multi segment, nextscore is logical -- use raw score or theta, 1 is for raw.
      nextcount=0, #nextcount = number of segments after current. if nextcount is positive, need thesholds then id's for each next segment after this point if multi segment.
      -20,	2,	2,	3,	4.5,	1,	4) #example next segment data

    out$test <- data.frame(key=1:ntests, scale= scale)

    out$item_ctg <- data.frame(item=1:nrow(adat), ctg=1)

    #linear segment control -- unused unless linear segment
    out$item_tst <- data.frame(item=1:nrow(adat),
      linsegment=-1, #segment id they belong to
      rank=0) #order of presentation

    out$item_scl <- data.frame(key=1:nrow(adat),item=1:nrow(adat),scale=scale,apar=round(adat$A,3),bpar=round(adat$B,3),
      fixed=2) #fixed > 2 for some form of fixed, 1 = uncalibrated, 0 = ignored, check values > 2
    out$item_scl <- cbind(out$item_scl,data.frame(matrix(0,nrow(out$item_scl),18))) #ECfreq(16),expfreq, clbfreq

    out$seg_ctg <- data.frame(segment=1,category=1,minitem=minlen,maxitem=maxlen,minperc=-1,maxperc=-1)

    for(i in 1:length(out)){
      n=file.path(folder,paste0(names(out)[i],'.txt'))
      write.table(nrow(out[[i]]),file=n,row.names=FALSE,col.names=FALSE, quote=FALSE)
      write.table(out[[i]],file = n,sep='\t',row.names=FALSE,col.names=FALSE,append = TRUE )
    }
    NULL
  }
