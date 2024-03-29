% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights.R
\name{insights}
\alias{insights}
\title{Model insights}
\usage{
insights(
  mfit,
  vars,
  data,
  interactions = "user",
  hcut = 0.75,
  pred_fun = NULL,
  fx_in = NULL,
  ncores = -1
)
}
\arguments{
\item{mfit}{Fitted model object (e.g., a "gbm" or "randomForest" object).}

\item{vars}{Character vector specifying the features to get insights on.}

\item{data}{Data frame containing the original training data.}

\item{interactions}{String specifying how to deal with interaction effects:
\describe{ \item{'user'}{specify interactions in \code{vars} as
\code{"var1_var2"}.} \item{'auto'}{automatic selection of interactions
based on \code{hcut}.} }}

\item{hcut}{Numeric in the range [0,1] specifying the cut-off value for the
normalized cumulative H-statistic over all two-way interactions, ordered
from most to least important, between the features in \code{vars}. Note
that \code{hcut = 0} will add the single most important interaction, while
\code{hcut = 1} will add all possible two-way interactions.}

\item{pred_fun}{Optional prediction function to calculate feature effects for
the model in \code{mfit}. Requires two arguments: \code{object} and
\code{newdata}. See \code{\link[pdp:partial]{pdp::partial}} and this
\href{https://bgreenwell.github.io/pdp/articles/pdp-extending.html}{article}
for the details. See also the function \code{gbm_fun} in the example.}

\item{fx_in}{Optional named list of data frames containing feature effects
for features in \code{vars} that are already calculated beforehand, to
avoid having to calculate these again. A possible use case is to supply the
main effects such that only the interaction effects still need to be
calculated. Precalculated interactions are ignored when \code{interactions
  = "auto"}, but can be supplied when \code{interactions = "user"}. It is
important to make sure that you supply the pure interaction effects.}

\item{ncores}{Integer specifying the number of cores to use. The default
\code{ncores = -1} uses all the available physical cores (not threads), as
determined by \code{parallel::detectCores(logical = 'FALSE')}.}
}
\value{
List of tidy data frames (i.e., "tibble" objects), containing the
partial dependencies for the features (and interactions) in \code{vars}.
}
\description{
Obtain insights from a black box model in the form of feature effects.
}
\examples{
\dontrun{
data('mtpl_be')
features <- setdiff(names(mtpl_be), c('id', 'nclaims', 'expo', 'long', 'lat'))
set.seed(12345)
gbm_fit <- gbm::gbm(as.formula(paste('nclaims ~',
                               paste(features, collapse = ' + '))),
                    distribution = 'poisson',
                    data = mtpl_be,
                    n.trees = 50,
                    interaction.depth = 3,
                    shrinkage = 0.1)
gbm_fun <- function(object, newdata) mean(predict(object, newdata, n.trees = object$n.trees, type = 'response'))
gbm_fit \%>\% insights(vars = c('ageph', 'bm', 'coverage', 'fuel'),
                     data = mtpl_be,
                     interactions = 'auto',
                     hcut = 0.75,
                     pred_fun = gbm_fun)
gbm_fit \%>\% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
                     data = mtpl_be,
                     interactions = 'user',
                     pred_fun = gbm_fun)
}
}
