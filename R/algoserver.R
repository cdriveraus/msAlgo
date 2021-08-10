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
    .res$set_body(msAlgo:::selectItem(scalename=.req$parameters_query$scalename,
      ability = .req$parameters_query$ability))

    .res$set_content_type('application/json')
  }

  abilityEst_rserve = function(.req, .res) {
    .res$set_body(msAlgo:::msFit(.req$parameters_query$jsontextdat))
    .res$set_content_type('application/json')
  }

  app$add_get(path = "/selectItem_rserve", FUN = selectItem_rserve)
  app$add_get(path = "/abilityEst_rserve", FUN = abilityEst_rserve)

  # request = Request$new(path = "/selectItem_rserve",
  #   parameters_query = list(scalename='maths',ability=.5))
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
