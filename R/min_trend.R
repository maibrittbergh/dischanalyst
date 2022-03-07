
#'Trend of minimum value
#'@description Calculates minimum Value for every year since the begin of the measurements. Uses least squares approach with a higher uncertainity and Sen-Sloap approach with "Yuepilon" PreWhitening (zyp-package).
#'
#' @param data  list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station. Must equal one entry of the data list.
#' @param mod numeric; possible input: 1,2,3. default value: 1; output of both: \link[zyp]{zyp.trend.vector}, \link[stats]{lm}. Defines the way to calculate intercept and slope. For mod=2  \link[zyp]{zyp.trend.vector} with PreWhitening by "yuepilon-method" is used. Sen-Slope-Approach used to define direction of the trend and the significance is  determined by Kendall's P-Value computed for the final detrendet time series. For mod=3: \link[stats]{lm} with a least squares approach is used.
#' @return list
#' \describe{
#'   \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
#'   \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
#'   \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
#'   \item{intercept_ls}{intercept created by \link[stats]{lm}}
#'   \item{slope_ls}{slope created by \link[stats]{lm}}
#' }
#'

#'
#' @export
#'@importFrom zyp zyp.trend.vector
#'@importFrom stats lm
#'
#' @examples
#' \dontrun{ min_trend(mosel, "TRIER UP")}

min_trend=function(data, station, mod= 1) {


  data=data[[station]]
  val=data[,2]
  abs_min=min(val)
  #Minima der Jahre
  year_one=as.numeric(substring(as.character(data[1,1]),1,4))

  last_year=as.numeric(substring(as.character(data[length(val),1]),1,4))
  years=c(year_one:last_year)
  l=length(years)
  q_min=rep(0, l)
  for ( i in 1:l){
    year=as.character(years[i])
    j=grep(year, data[,1])
    Val=data[,2][j]
    q_min[i]=min(Val)
  }
  results=data.frame(years, q_min)

  if (mod == 3){


    model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
    intercept_ls=as.numeric(model$coefficients[1])
    slope_ls=as.numeric(model$coefficients[2])
    llm=list(intercept_ls, slope_ls)
    names(llm)=c("intercept_lm", "slope_lm")
    return(llm)

  }else if (mod == 2){
    mod=zyp.trend.vector(y=results$q_min, x=years,  method="yuepilon")  #
    intercept_zyp=as.numeric(mod[11])
    slope_zyp=as.numeric(mod[2])
    sig_zyp=as.numeric(mod[6])
    lzyp= list(intercept_zyp, slope_zyp, sig_zyp)

    names(lzyp)=c("intercept_zyp", "slope_zyp","sig_zyp")


    return(lzyp)



  }else{


    model=lm(q_min ~ years, results)  #least squares.lm to fit a linear model.
    intercept_ls=as.numeric(model$coefficients[1])
    slope_ls=as.numeric(model$coefficients[2])

    mod=zyp.trend.vector(results$q_min, x=years, method="yuepilon")  #
    intercept_zyp=as.numeric(mod[11])
    slope_zyp=as.numeric(mod[2])
    sig_zyp=as.numeric(mod[6])
    lb= list(intercept_zyp, slope_zyp, sig_zyp, intercept_ls, slope_ls)

    names(lb)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm")


    return(lb)
  }


}


