% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/segmentation.R
\name{segmentation}
\alias{segmentation}
\title{Data segmentation}
\usage{
segmentation(fx_vars, data, type, values, max_ngrps = 15)
}
\arguments{
\item{fx_vars}{List of data frames containing the feature effects.}

\item{data}{Data frame containing the original training data.}

\item{type}{String specifying the type of segmentation. Options are:
\describe{ \item{'ngroups'}{the number of groups to use for grouping the
features.} \item{'lambdas'}{optimal number of groups determined by
penalized loss.} }}

\item{values}{The values for \code{ngroups} or \code{lambdas}. This can be a
numeric value (same is used for all features in \code{fx_vars}) or a named
numeric vector of \code{length(fx_vars)} (for feature-specific values). In
this case, the names must match the comment attributes in \code{fx_vars}.}

\item{max_ngrps}{Integer specifying the maximum number of groups that each
feature's values/levels are allowed to be grouped into. Only used when
determinining the optimal number of groups via \code{type = 'lambdas'}.}
}
\value{
Data frame with the segmented data. The grouped features are added to
the original \code{data} and have a trailing underscore in their name.
}
\description{
Segmentation of observations based on the grouping of feature effects.
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
gbm_fit \%>\% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
                     data = mtpl_be,
                     interactions = 'user',
                     pred_fun = gbm_fun) \%>\%
            segmentation(data = mtpl_be,
                         type = 'ngroups',
                         values = setNames(c(7, 8, 2, 2, 3), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel')))
}
}
