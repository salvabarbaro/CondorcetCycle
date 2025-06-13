########################### Functions ################################

CoreRegression <- function(vars, data, FE="0"){

    # Function runs following 4 regressions and stores in list
    # 1. with only Core and CWin
    # 2. with core and controls
    # 3. with Cwin and controls
    # 4. with both core and Cwin and with controls

    # INPUT:
    #       vars      - vector of varnames, first var is DV, rest are IV
    #       data      - dataframe
    #       FE        - list of fixed effects, default is none
    # OUTPUT:
    #       A list of all regression models

    dv <- vars[1]
    iv <- vars[2:length(vars)]

    models <- rbind(c(1,1,rep(0,length(iv) - 2)),
                     c(1,0,rep(1,length(iv) - 2)),
                     c(0,1,rep(1,length(iv) - 2)),
                     rep(1,length(iv)))
    models <- models==1

    CoreRegression <- list()

    for (i in 1:nrow(models)){
        formula <- as.formula(paste(paste(dv," ~ "),paste(iv[models[i,]],
                    collapse= "+"),paste("|",paste(FE, collapse = "+"),"| 0 | id")))
        CoreRegression[[i]] <- felm(formula = formula, data = data)
    }

CoreRegression

}

# Other auxiliary functions for dataframe manipulation

fully <- function(x,y){ifelse(x == 1 & y == 1, 1, 0)}
possibly <- function(x,y){ifelse((x == 1 & y == 0) | (x == 0 & y == 1), 1, 0)}
notc <- function(x,y){ifelse(x == 0 & y == 0, 1, 0)}

matsplitter <- function(m,p) {
    cv <- Map(function(u,v) m[,u:v], seq(1,ncol(m),p), seq(p,ncol(m),p))
    cv
}

# Functions to print results

printsumtable <- function(table,text){
    cat(paste('\n\n\n\n',text,'\n\n'))
    print(table)
    cat('\n\n\n\n')
}

printregtable <- function(table,coef,text){
    cat(paste('\n\n',text,'\n\n'))
    stargazer(table,covariate.labels = coef,
    type="text",omit.stat=c("ser","f"))
    cat('\n\n\n\n')
}
