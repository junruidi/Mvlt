#' @title Create all possible movelets from the tri-axial data
#' @description This function takes the tri-axial data and decomposes them into pieces of movelets. It creates the object of class "Mvlt".
#'
#' @param DATA \code{data.frame} with four columns: Timestamp, x, y, z, and Label.
#' @param Length window size for each movelet (the most inuitive choise can be the sampling frequency).
#' @param FROM row index of the start time to extract from \code{DATA}.
#' @param TO row index of the end time to extract from \code{DATA}.
#'
#'
#' @return An object of class "Mvlt", which is a list of
#' \item{Movelet}{A collection of all possible movelets extraced. A list of length 3 matrices corresponding to the 3 axes. The dimension of each matrix is number of movelets by the window size}
#' \item{Movelet_lab}{Labels of all the movelets}
#'
#' @export
#'


# becomes tricky now if we do it across whole night
Mvlt_Create = function(DATA, FROM = 1, TO = nrow(DATA), Length){

  if (is.null(Length) == TRUE) {
    stop("Need to provide the length of the movelet!")
  }
  Dim.Collection = TO - Length + 1 - FROM + 1
  if (Dim.Collection < 1) {
    stop("Movelet length longer than time used!")
  }

  #
  Collection = list(AX1 = array(0, c(Dim.Collection, Length)),
                    AX2 = array(0, c(Dim.Collection, Length)),
                    AX3 = array(0, c(Dim.Collection, Length)))

  temp = c(1:(TO - FROM + 1))
  IndexMatrix = c()
  for (t in (0:(Length - 1))) {
    IndexMatrix = c(IndexMatrix, temp[(1 + t):(length(temp) - (Length - t) + 1)])
  }
  ax = c("x","y","z")
  for (j in 1:3) {
    Collection[[j]] = matrix(c(t(DATA[IndexMatrix + FROM - 1, ax[j]])), ncol = Length)
  }


  timeLab = data.frame(Timestamp = DATA$Timestamp[IndexMatrix[1:Dim.Collection]], Label = DATA$Label[IndexMatrix[1:Dim.Collection]])
  timeLab$Timestamp = as.character(timeLab$Timestamp)
  timeLab$Label = as.character(timeLab$Label)

  Result = list(Movelet = Collection, Movelet_lab = timeLab)
  class(Result) = "Mvlt"
  return(Result)

}
