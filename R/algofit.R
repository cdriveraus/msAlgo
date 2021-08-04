msFit <- function(jsontextdat){
  return(jsontextdat)
  # adat=as.data.table(jsonlite::fromJSON(jsontextdat))
  # Ability=bigIRT::fitIRT(
  #   dat = adat,
  #   score='score',id = 'id',
  #   item = 'item',scale = 'Scale',pl = 2,
  #   cores=1,  priors = TRUE,ebayes = FALSE,itemDat = adat,
  #   normalise = FALSE,dropPerfectScores = FALSE)$pars$Ability
}

# setwd("/home/driver/bigIRT/testing/")
# load(file='adat.rda');
# require(data.table)
# # require(bigIRT)
# # con <- socketConnection(host = "localhost", port = 8888, #connect to socket server
# #   blocking = FALSE, timeout = 3)
# #
# # svSocket::evalServer(con,adat2,as.data.frame(adat))
# #
# # Ability= svSocket::evalServer(con,
# # Ability=bigIRT::fitIRT(dat = adat,score='score',id = 'id',item = 'item',scale = 'Scale',pl = 2,cores=1,  priors = TRUE,ebayes = FALSE,itemDat = adat2,normalise = FALSE,dropPerfectScores = FALSE)$pars$Ability
# # )
#
# # svSocket::evalServer(con, 'done <- NULL')
#
#
#
