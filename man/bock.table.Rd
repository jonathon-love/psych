\name{bock}
\alias{bock}
\alias{bock.table}
\alias{lsat6}
\alias{lsat7}
\alias{bock.lsat}
\docType{data}
\title{Bock and Liberman (1970) data set of 1000 observations of the LSAT
}
\description{An example data set used by McDonald (1999) as well as other discussions of Item Response Theory makes use of a data table on 10 items (two sets of 5) from the Law School Admissions Test (LSAT).  Included in this data set is the original table as well as the reponses for 1000 subjects on the first set (Figure Classification) and second set (Debate). 
}
\usage{data(bock)}
\format{
  A data frame with 32 observations on the following 8 variables.
  \describe{
    \item{\code{index}}{32 response patterns}
    \item{\code{Q1}}{Responses to item 1}
    \item{\code{Q2}}{Responses to item 2}
    \item{\code{Q3}}{Responses to item 3}
    \item{\code{Q4}}{Responses to item 4}
    \item{\code{Q5}}{Responses to item 5}
    \item{\code{Ob6}}{count of observations for the section 6 test}
    \item{\code{Ob7}}{count of observations for the section 7 test}
    }
    Two other data sets are derived from the bock dataset. These are converted using the \code{\link{table2df}} function.
    \describe{
    \item{lsat6}{reponses to 5 items for 1000 subjects on section 6}
    \item{lsat7}{reponses to 5 items for 1000 subjects on section 7}
       }
   }
\details{The lsat6 data set is analyzed in the ltm package as well as by McDonald (1999). lsat7 is another 1000 subjects on part 7 of the LSAT. Both sets are described by Bock and Lieberman (1970). Both sets are useful examples of testing out IRT procedures and showing the use of \code{\link{tetrachoric}} correlations and item factor analysis using the \code{\link{irt.fa}} function.
    }
\source{
R. Darrell Bock and M. Lieberman (1970). Fitting a response model for dichotomously scored items. Psychometrika, 35(2):179-197.
}
\references{
R.P. McDonald. Test theory: A unified treatment. L. Erlbaum Associates, Mahwah, N.J., 1999.
}
\examples{
data(bock)
responses <- table2df(bock.table[,2:6],count=bock.table[,7],
        labs= paste("lsat6.",1:5,sep=""))
describe(responses)
## maybe str(bock.table) ; plot(bock.table) ...
}
\keyword{datasets}
