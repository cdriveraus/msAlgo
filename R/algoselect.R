if(F) itemcode <- bigIRT:::selectItem(items,
  ability = .2, # unlist(persons[person,scale,with=FALSE]),
  targetease = 0.1,samplesize = 1)

selectItem <- function(items, scalename,ability, targetease=.1, samplesize=1){
  sample(items[Scale %in% scalename,]$Item[order(abs(ability-items$B-targetease))[1:min(nrow(items),samplesize)]],size = 1) #could use expected information instead
}
