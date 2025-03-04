# match_from_dots <- function(dots, fn){ #SOURCE: https://community.rstudio.com/t/dots-vs-arg-lists-for-function-forwarding/4995
#   arg <- match(names(formals(fn)), names(dots))
#   dots[arg[!is.na(arg)]]
# }

match_from_dots <- function(dots, fn,skip=NULL){
  arg <- match(names(formals(fn)), names(dots))
  #print(arg)
  dots<-dots[arg[!is.na(arg)]]
  dots<-dots [!names(dots)%in%skip]
  #print(dots)
  dots
}