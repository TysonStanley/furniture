#' NHANES 2009-2010
#'
#' A dataset containing information on health, healthcare, and demographics of adolescents
#' aged 12 - 19 in the United States from 2009 to 2010. This is a cleaned dataset 
#' which is only a subset of the 2009-2010 data release of
#' the National Health and Nutrition Examination Survey (NHANES).
#'
#' @format A data frame with 4727 rows and 10 variables:
#' \describe{
#'   \item{seqn}{individual ID}
#'   \item{year}{year of data release (2006, 2008, 2010)}
#'   \item{gender}{gender of the individual (factor with "male" or "female")}
#'   \item{raceth}{race/ethnicity of the individual (factor with "MexAm", "OtherHisp", "White", "Black", "Other")}
#'   \item{age}{age of individual (numeric)}
#'   \item{income}{income as a ratio of income-to-poverty line}
#'   \item{fmsize}{the size of the individual's family (up to 6)}
#'   \item{limits}{whether the individual has physical limitations (factor 1 = Limitations, 0 = No Limitations)}
#'   \item{medcond}{whether the individual has at least one medical condition [i.e. asthma, psoriasis, overweight, celiac, and/or trouble seeing] (factor 1 = Yes, 0 = No)}
#'   \item{hinsur}{whether the individual has health insurance (factor 1 = insured, 0 = not insured)}
#' }
#' @source \url{http://wwwn.cdc.gov/nchs/nhanes/search/nhanes09_10.aspx}
"nhanes10"