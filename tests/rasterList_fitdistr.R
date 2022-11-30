# This is a test script for rasterList function
# 
# Author: Emanuele Cordano
###############################################################################
rm(list=ls())

library(rasterList)
library(lmom)
library(lubridate)


## TESTING R CODE: 
library(testthat)
context("Verfiy Probabibilty Distribution")

## It is Raster Examples:

precf <- system.file("map/precipitation.grd", package="rasterList")

prec <- stack(precf)

### Or you can use Mekrou examples: 
## Yearly Precipitaion on Mecrow

precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
pvalf <- system.file("map/Mekrou_pvalkstest.grd", package="rasterList")


prec <- stack(precf)
pval <- raster(pvalf)

## Set time

time <- as.Date(names(prec),format="X%Y.%m.%d")
year <- year(time) ##lubridate::yearas.character(time,format="X%Y")

## Compute Annual Precipitation (sum aggregration)
yearlyprec <- stackApply(x=prec,fun=sum,indices=year)

##
print(yearlyprec)
## L-moments


samlmom <- stack(rasterList(yearlyprec,FUN=samlmu))


print(samlmom)
##
## lmrd plot 

lmrd(as.data.frame(samlmom),cex=0.3)


## 
## These are all parametric distribution described in "lmom" packages: 
## distribs <- c("exp","gam","gev","glo","gno","gpa","gum","kap","ln3","nor","pe3","wak","wei")
distribs <- c("gam","pe3")
pels <- paste("pel",distribs,sep="")
cdfs <- paste("cdf",distribs,sep="")
names(pels) <- distribs
names(cdfs) <- distribs
##
##
##
nn <- names(pels)

cdfs <- cdfs[nn]
pels <- pels[nn]

## FIT AND KSTESTING 
fitPrec_gam <- rasterList(samlmom,FUN=pelgam)

print(fitPrec_gam@list)


##
kstest_gam  <- RasterListApply(x=rasterList(yearlyprec),para=fitPrec_gam,y="cdfgam",FUN=ks.test)

###
print(kstest_gam@list)
###
pvalkstest <- raster(rasterList(kstest_gam,FUN=function(x) {return(x$p.value)}))	

## 
print(pvalkstest)
##


test <- values(pvalkstest-pval)
str(test)
test0 <- rep(0,length(test))

#
#####
#xxx <- '/STORAGE/projects/R-Packages/rasterList/inst/map/Mekrou_pvalkstest.grd' 
#
#writeRaster(pvalkstest,file=xxx,overwrite=TRUE)
#
#
#####
test_that(desc="Testing final Results",code=expect_equal(test,test0, tolerance = .02, scale = 1))

#
#fitPrec <- rasterList(samlmom,FUN=function(x,pels=pels) {
#			
#			o <- lapply(X=pels,FUN=function(pel,lmom) {
#						
#						o <- try(do.call(what=get(pel),args=list(lmom=lmom)),silent=TRUE)
#						
#						if (class(o)=="try-error") o <- NULL
#						return(o)
#					},lmom=x)
#			
#			inull <- which(sapply(X=o,FUN=is.null))
#			
#			##o <- o[-inull]
#			
#			return(o)
#			
#		},pels=pels)
#
#
#### Kolgormov-Smirnov Test for all available parametric distributions with the parameters calculated in fitPrec
#
#ksTest <- RasterListApply(x=fitPrec,val=rasterList(yearlyprec),FUN=function(x,val,...){
#			
#		
#			o <- x
#			inull <- which(sapply(X=o,FUN=is.null))
#			x <- x[-inull]
#			
#			ndistr <- names(x)
#			cdfv <- paste("cdf",ndistr,sep="")
#			names(cdfv) <- names(x)
#			
#		###	parav <- lapply(X=x,FUN=list)
#		###	parav <- lapply(X=parav,FUN=function(x){names(x)[1] <- "para"})
#			
#			parav <- mapply(para=x,cdfv=cdfv,FUN=list,USE.NAMES=TRUE,SIMPLIFY=FALSE)
#			print(parav)
#			##print(val)
#			kstest <- NA
#			kstest <- lapply(X=parav,FUN=function(p,val){
#						#print(val)
#						#print(p)
#						o <- ks.test(x=val,y=p$cdfv,para=p$para,...)
#						return(o)
#					},val=val,...)
#			
#			o[-inull] <- kstest
#			
#			return(o)
#			
#		})
#
#
####
#
#
#pvalkstest <- rasterList::stack(rasterList(ksTest,FUN=function(x){  
#		
#			print(x)
#			o <- sapply(X=x,FUN=function(t){
#						print(t)
#						###return(NA)
#						return(t$p.value)
#					})
#			return(o)
#		}))
#
#
#### SAVE THE RESULTS 
#
#
