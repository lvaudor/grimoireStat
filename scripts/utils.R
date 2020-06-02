to_bins=function(mydf,var,qmin,qmax,Nbins){
  if(!("frame"%in%colnames(mydf))){mydf=mutate(mydf,frame=1)}
  df_basis=bind_cols(frame=rep(unique(mydf$frame),Nbins-1),
                     id=1:(Nbins-1),
                     Xmin=seq(qmin,qmax,length.out=Nbins)[1:(Nbins-1)],
                     Xmax=seq(qmin,qmax,length.out=Nbins)[2:Nbins])
  fcount=function(x){
    Y=length(which(mydf[[var]]>x$Xmin & mydf[[var]]<=x$Xmax))
    result=bind_cols(x,Y=Y)
    return(result)
  }
  df_result=df_basis %>%
    mutate(iter=id) %>%
    group_by(iter) %>%
    nest() %>%
    extract2("data") %>%
    map(fcount) %>%
    bind_rows() %>%
    mutate(Y=Y/max(Y))
  partdf=bind_rows(df_result,df_result) %>% select(-Xmin,-Xmax,-Y)
  result=bind_cols(partdf,
                   X=c(df_result$Xmin,df_result$Xmax),
                   Y=rep(df_result$Y,2)) %>%
    arrange(id,X) %>%
    select(-id)

  return(result)
}

repeat_identical=function(mydf,frame){
 mydf=map(as.list(frame),
          ~bind_cols(frame=rep(.,nrow(mydf)),
                     mydf)) %>%
    bind_rows()
 return(mydf)
}
distribution_of_mean=function(N,
                              mu,
                              sigma,
                              nIter=300,
                              nImages=30,
                              model="norm",
                              show=c(TRUE,TRUE,TRUE,TRUE),
                              animate=TRUE){
  dfun=get(paste0("d",model))
  qfun=get(paste0("q",model))
  rfun=get(paste0("r",model))
  qmin=qfun(0.01,mu,sigma)
  qmax=qfun(0.99,mu,sigma)
  xd=seq(qmin,qmax,length.out=300)
  #####
  dfX=tibble(frame=1:nIter) %>%
    group_by(frame) %>%
    nest() %>%
    extract2("frame") %>%
    map(.f=function(frame){
      X=rfun(N,mu,sigma)
      df=tibble(frame=frame,
                X=X)
      return(df)
    }) %>%
    bind_rows() %>%
    group_by(frame) %>%
    mutate(Xbar=mean(X)) %>%
    ungroup()
  #####
  dfXplot=dfX %>%
    filter(frame<=nImages) %>%
    mutate(iter=frame) %>%
    group_by(iter) %>%
    nest() %>%
    extract2("data") %>%
    map(to_bins,var="X",qmin=qmin,qmax=qmax,Nbins=20) %>%
    bind_rows()
  #####
  dfXbar=dfX %>%
    select(Xbar) %>%
    unique() %>%
    to_bins(var="Xbar",qmin=qmin,qmax=qmax,Nbins=100) %>%
    repeat_identical(frame=1:nImages)
  #####
  dfXTheo=tibble(xd=xd,
                 yd=dfun(xd,mu,sigma)) %>%
    repeat_identical(1:nImages) %>%
    mutate(yd=yd/max(yd))
  #######
  dfX=filter(dfX,frame<=nImages)
  xlarge=rfun(100000,mu,sigma)
  meantheo=mean(xlarge)
  sdtheo=sd(xlarge)
  dfXbarTheo=tibble(xd=xd,
                    yd=dnorm(xd,meantheo,sdtheo/sqrt(N))) %>%
    repeat_identical(1:nImages) %>%
    mutate(yd=yd/max(yd))
  #########
  panim=ggplot(dfX,aes(x=X))+
    scale_x_continuous(limits=c(qmin,qmax))+
    scale_y_continuous(labels=NULL)+
     ggtitle(bquote(paste(X," de loi ",.(model)(mu==.(mu),sigma==.(sigma))," avec ",N==.(N))))
  if(show[1]){
    panim=panim+
      geom_ribbon(data=dfXTheo,
                  aes(x=xd,ymin=0,ymax=yd),
                  fill="forestgreen", alpha=0.25)
  }
  if(show[2]){
    panim=panim+
      geom_ribbon(data=dfXplot,
                  aes(x=X,ymax=Y,ymin=0,frame=frame),
                  bins=N,
                  col="forestgreen", alpha=0.25)+
      geom_vline(aes(xintercept=Xbar,frame=frame), col="red")
  }
  if(show[3]){
    panim=panim+
      geom_ribbon(data=dfXbarTheo,
                  aes(x=xd,ymin=0,ymax=yd),
                  fill="red", alpha=0.25)
  }
  if(show[4]){
    panim=panim+
      geom_ribbon(data=dfXbar,
                  aes(x=X,ymax=Y, ymin=0),
                  col="red", alpha=0.25
      )
  }
  result=panim
  if(animate){result=gganimate(panim,interval=0.3,title_frame = F)}

  return(result)
}
