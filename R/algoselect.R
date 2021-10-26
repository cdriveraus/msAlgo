if(F) itemcode <- bigIRT:::selectItem(items,
  ability = .2, # unlist(persons[person,scale,with=FALSE]),
  targetease = 0.1,samplesize = 1)

#' Title
#'
#' @param items
#' @param scalename
#' @param ability
#' @param targetease
#' @param samplesize
#'
#' @return
#' @export
#'
#' @import data.table bigIRT
#'
#' @examples
selectItem <- function(scalename,ability, items=msAlgo::items, targetease=.1, samplesize=1){
  if(!'data.table' %in% class(items)) stop('items should be a data.table!')
  # items=data.frame(items)
  isub=items[Scale %in% scalename,]
  sample(isub$Item[
    order(abs(ability-isub$B-targetease))[1:min(nrow(isub),samplesize)]
    ],size = 1) #could use expected information instead
}

# remotes::install_github('cdriveraus/msAlgo', INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))

