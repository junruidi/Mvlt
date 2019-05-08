#' @title Create chapters from the extracted movelet
#'
#' @description This functions takes the object "Mvlt" with all movelets and creates different chapters
#'
#' @param Mvlt: object "Mvlt" created by \code{Mvlt_Create}.
#' @param ChapName: The name of the chapter.
#'
#' @return An object of class "MvltChap", which is a list of
#' \item{Chapter}{All movelets describing this chapter}
#' \item{ChapName}{The name of the chapter}
#'
#' @export
#'
#'

Chapter_Create = function(Mvlt, ChapName){
  chap.list = as.character(unique(Mvlt$Movelet_lab$Label))
  if(! ChapName %in% chap.list){
    stop("The chapter name is not listed in the labels!")
  }


  Chap = lapply(Mvlt$Movelet, function(x) x[which(Mvlt$Movelet_lab$Label == ChapName),])

  Result = list(Chapter = Chap, ChapName = ChapName)
  class(Result) = "MvltChap"

  return(Result)

}
