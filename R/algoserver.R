if(F){
  library(RestRserve)
  library(data.table)
  library(msAlgo)
  app = Application$new()#content_type = 'application/json')

  selectItem_rserve = function(.req, .res) {
    # args <-
    # if (length(n) == 0L || is.na(n)) {
    #   raise(HTTPError$bad_request())
    # }
    .res$set_body(msAlgo:::selectItem(scalename=as.character(.req$parameters_query$scalename),
      ability = as.numeric(.req$parameters_query$ability)))

    .res$set_content_type('application/json')
  }

  abilityEst_rserve = function(.req, .res) {
    .res$set_body(msAlgo:::msFit(.req$parameters_query$jsontextdat))
    .res$set_content_type('application/json')
  }

    test_rserve = function(.req, .res) {
    .res$set_body((as.numeric(.req$parameters_query$par1)+as.numeric(.req$parameters_query$par2)))
    .res$set_content_type('application/json')
  }

  app$add_get(path = "/selectItem_rserve", FUN = selectItem_rserve)
  app$add_get(path = "/abilityEst_rserve", FUN = abilityEst_rserve)
  app$add_get(path = "/test_rserve", FUN = test_rserve)

  #init objects
items <- msAlgo::items

Ability=bigIRT::fitIRT(dat = adat,score='score',id = 'id',
        item = 'item',scale = 'Scale',pl = 2,
        cores=1,  priors = TRUE,ebayes = FALSE,itemDat = adat,
        normalise = FALSE,dropPerfectScores = FALSE)$pars$Ability
rm(Ability)
selectItem('english',0.1)

  # system(intern = TRUE, command = curl localhost:8080/fib?n=10

  # request = Request$new(path = "/selectItem_rserve",
    # parameters_query = list(scalename='maths',ability=.5))
  # response = app$process_request(request)
  #
  # cat("Response status:", response$status)
  # #> Response status: 200 OK
  # cat("Response body:", response$body)
  #
  # jsonlite::fromJSON(response$body)
  #
  #
  #
  #   request = Request$new(path = "/abilityEst_rserve",
  #   parameters_query = list(jsontextdat=jsonlite::toJSON(adat)))
  # response = app$process_request(request)
  #
  # cat("Response status:", response$status)
  # #> Response status: 200 OK
  # cat("Response body:", response$body)
  #
  # jsonlite::fromJSON(response$body)




  backend = BackendRserve$new()
  backend$start(app, http_port = 8080)
}
