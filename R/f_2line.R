#' Drawing line charts of two categories or single category versus cross-sectional benchmark
#'
#' The function \code{f_2line} draws line charts of two categories or single category versus cross-sectional benchmark for cross-sections of a panel
#'
#' @param df zoo dataframe, containing all series of interest
#' @param cids character (vector), contains one or several cross-sections
#' @param cats character (vector), contains one or two categories
#' @param bm character, cross-sectional benchmark, default is "GLB", i.e. average is created (also for 2-categories case)
#' @param dax logical, TRUE means that series have two different axes
#' @param title character string, optional chart title, default is single cross section or category acronym
#' @param selout character (vector), default is NULL, contain the cross-sections for which we want to display the output \cr
#' i.e. can be diffrent from cids
#' @param leg1 character string, optional legend for line 1 (blue), default is panel member acronym
#' @param leg2 character string, optional legend for line 2 (dotted black), default is panel member acronym
#' @param size numeric, scales size of title and legend relative to all other graphical elements
#' @param showzero boolean, showing thick zero line, default is TRUE
#' @param devnew boolean, whether to open new graphic device for each graph (defalut=\code{FALSE} for RStudio use)
#'
#' @details
#'
#' @return
#'
#' @export
#'
#'@examples
#' require(zoo)
#' dfm <- zoo(matrix(rnorm(288, -5, 5), 48, 6), seq(as.Date("2012-02-01"), length=48, by="1 month") - 1)
#' names(dfm) <- as.vector(outer(c("EUR", "USD", "JPY"), c("V1", "V2"), paste, sep="_"))
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = c("V1", "V2"), dax=TRUE)
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = "V1", bm = c("EUR", "JPY"), dax=T)
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = "V1", bm = c("EUR"), dax=T)
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = c("V1", "V2"), bm = "JPY")
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = c("V1", "V2"), bm = "GLB", selout = c("EUR", "GLB"))
#' f_2line(dfm, c("EUR", "USD", "JPY"), cats = c("V1", "V2"), bm = "GLB", selout = c("GLB"))
#'
f_2line <- function(df, cids, cats, bm="GLB", dax=FALSE, title=NULL, selout=NULL, leg1=NULL, leg2=NULL, size=1, showzero=TRUE, devnew=FALSE){

  #A. Preparations
  stopifnot(length(cats) <= 2)
  df.xs <- df[, as.vector(outer(cids, cats, paste, sep="_")), drop=FALSE]
  if(bm=="GLB"){ bench <- cids }else if(length(bm)>1){ bench <- bm ;  bm <- "AVG"}else{ bench <- bm ; cids <- setdiff(cids, bm) }
  #bm <- ifelse(length(bench)>1, "GLB", bench)

  if(length(bench)>1 && length(cats) == 1){  # global benchmark for single category
    df.xs <- merge(df.xs, zoo(apply(df.xs[, paste(bench, cats, sep="_"), drop=FALSE], 1, mean, na.rm=T), index(df.xs)))
    names(df.xs)[ncol(df.xs)] <- paste(bm, cats, sep="_")
  }else if(length(bench)>1 && length(cats) == 2){  # global averages for double categories
    df.xm <- merge(zoo(apply(df.xs[, paste(bench, cats[1], sep="_"), drop=FALSE], 1, mean, na.rm=T), index(df.xs)),
                   zoo(apply(df.xs[, paste(bench, cats[2], sep="_"), drop=FALSE], 1, mean, na.rm=T), index(df.xs)))
    names(df.xm) <- paste(bm, cats, sep="_")
    df.xs <- merge(df.xs, df.xm)
    cids <- c(cids, bm)
  }
  #if(bm!="GLB") cids <- setdiff(cids, bm)

  #B. Loop through chart making

  if(!is.null(selout)) idloop <- selout else idloop <- cids

  for (c in 1:length(idloop)){
    if(length(cats) > 1){
      cv.ser <- paste(idloop[c], cats, sep="_")
      if(is.null(title)) main <- idloop[c] else main <- paste(idloop[c], title, sep=" : ")
      if(is.null(leg1)) l1 <- cats[1] else l1 <- leg1
      if(is.null(leg2)) l2 <- cats[2] else l2 <- leg2
    }else{
      cv.ser <- paste(c(idloop[c], bm), cats, sep="_")
      if(is.null(title)) main <- cats else main <- title
      if(is.null(leg1)) l1 <- idloop[c] else l1 <- leg1
      if(is.null(leg2)) l2 <- bm else l2 <- leg2
    }
    df.ser <- na.trim(df.xs[, cv.ser], is.na="any")
    d.f <- index(df.ser)[match("01", format(index(df.ser), "%m"))]
    d.l <- tail(index(df.ser),1)
    if(devnew) dev.new()
    par(xpd=TRUE, mar=c(8,2.5,3.5,2.5))  #enables to draw/write outside of the plot area, and sets the margin widths
    range1 <- range(df.ser[, 1], na.rm=TRUE)
    range2 <- range(df.ser[, 2], na.rm=TRUE)
    if(dax){  #double-axis chart
      if(range1[1] * range1[2] < 0 && range2[1] * range2[2] < 0){
        M <- Mnew <- cbind(range1, range2)
        Mratio <- abs(M) / rep(diff(M),each=2)
        nmin <- which(Mratio == min(Mratio))
        Mscale <- array(rep(rev(Mratio[ifelse(nmin%%2==0,2,1),]/Mratio[ifelse(nmin%%2==0,1,2),]), each=2), dim=c(2,2)) * apply(abs(M), 2, rev) * sign(M)
        Mnew[nmin] <- Mscale[nmin]
        range1 <- Mnew[,1]
        range2 <- Mnew[,2]
      }
      plot(df.ser[, 1, drop=FALSE], xlab="", ylab="", ylim=range1, main=main, col="blue", xaxt='n', cex.main=size)
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray95", border=NA)  #sets grey only in the plot area
      lines(df.ser[, 1, drop=FALSE], col="blue")  #one has to draw the first series anew
      if(showzero && range1[1] * range1[2] < 0) lines(zoo(rep(0, length(index(df.ser))), index(df.ser)), col="black", lty=1, lwd=2)
      par(new=T)
      plot(df.ser[, 2], axes=F, ylim=range2, ylab="", main=main, col="black", lty="dashed", type="l", ann=FALSE, cex.main=size)
      axis(4, ylab=l2)
      yleg=par("usr")[3]-(par("usr")[4]-par("usr")[3])/15
      legend(	x=par("usr")[1], y=yleg, c(paste(l1, "(LHS)"), paste(l2, "(RHS)")), text.col=c("blue","black"), lty=c("solid","dashed"), col=c("blue", "black"),
              cex=size, bty="n", y.intersp=0.8)  #legend placed under the graph, with noborders
      par(new=F)
    }else{  # single-axis chart
      plot(df.ser[, 1, drop=FALSE], xlab="", ylab="", main=main, ylim=range(df.ser, na.rm=T), col="blue", xaxt='n', cex.main=size)
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray95")  #sets gery only in the plot area
      lines(df.ser[, 1, drop=FALSE], col="blue")  #one has to draw the first series anew
      lines(df.ser[, 2, drop=FALSE], col="black", lty="dashed")
      if(showzero && range1[1] * range1[2] < 0) lines(zoo(rep(0, length(index(df.ser))), index(df.ser)), col="black", lty=1, lwd=2)
      yleg=par("usr")[3]-(par("usr")[4]-par("usr")[3])/15
      legend(	x=par("usr")[1], y=yleg, c(l1, l2), text.col=c("blue", "black"), lty=c("solid","dashed"), col=c("blue", "black"),
              cex=size, bty="n", y.intersp=0.8)  #legend placed under the graph, with noborders
    }
    axis.Date(1, at=seq(from=d.f, to=d.l,by="12 months"), format="%b-%Y", cex=size)  # dates axis
    par(xpd=FALSE)  #disable drawing outside the plotting area, to draw clean gridlines
    abline(v=seq(from=d.f, to=d.l, by="12 months"), col="darkgray", lty=3)  # gridlines
  }
}
