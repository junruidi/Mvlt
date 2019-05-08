#' @title Compute the Euclidean distances betweem a collection of unlabeled movelets and a known chapter
#'
#' @description This function calculate the distance matrix between an unlabeled chapter of movelets to a labeled chapter of movelts
#'
#' @param Chapter The labeled chapter of movelets
#' @param Unlabel The unlabled chapter of movelts
#'
#' @return
#' Distance of each movelet in Unlabeled from the movelet chapter Chapter. If index==TRUE, it also returns the indices of their best matches in Chapter.


#'

Calc_Dist = function(Chapter, Unlabel){
  if (class(Chapter) != "MvltChap") {
    stop("Please use appropriate object for Chapter!")
  }
  if (class(Unlabel) != "MvltChap") {
    stop("Please use appropriate object for Unlabeld data!")
  }

  N_Chapter = nrow(Chapter$Chapter$AX1)
  N_Sample = nrow(Unlabel$Chapter$AX1)

  Distance = array(0, c(N_Sample, N_Chapter))
  for (i in 1:N_Chapter) {
    Dist1 = sqrt(apply((t(t(Unlabel$Chapter$AX1) -
                            Chapter$Chapter$AX1[i, ]))^2, 1, sum))
    Dist2 = sqrt(apply((t(t(Unlabel$Chapter$AX2) -
                            Chapter$Chapter$AX2[i, ]))^2, 1, sum))
    Dist3 = sqrt(apply((t(t(Unlabel$Chapter$AX3) -
                            Chapter$Chapter$AX3[i, ]))^2, 1, sum))
    Dist = (Dist1 + Dist2 + Dist3)/3
    Distance[, i] = Dist
  }
  Result = array(0, c(2, N_Sample))
  Result[1, ] = apply(Distance, 1, which.min)
  Result[2, ] = apply(Distance, 1, min)

  return(Result)
}
