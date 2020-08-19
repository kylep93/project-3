colley <- function(year){
  
  #download data
  q<-read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",year,"gms.txt",sep=""),c(10,29,2,30,2,50))
  q$V2=as.character(gsub(" ","",q$V2))
  q$V4=as.character(gsub(" ","",q$V4))
  
  #count number of games for each team and drop teams with less than 6  
  awaygames = summarise(group_by(q,V2),num=n())
  homegames = summarise(group_by(q,V4),num=n())
  totalgames = full_join(awaygames,homegames,by=c(V2="V4"))
  totalgames$num.x[is.na(totalgames$num.x)]=0
  totalgames$num.y[is.na(totalgames$num.y)]=0
  totalgames$games = totalgames$num.x+totalgames$num.y
  totalgames=totalgames[totalgames$games>6,]
  
  teamstokeep = totalgames$V2
  
  q = q[(q$V2 %in% teamstokeep)&(q$V4 %in% teamstokeep),]
  
  #determine which team won each game and drop ties
  q$homewin = q$V5>q$V3
  q$tie = q$V5==q$V3
  
  q = q[!q$tie,]
  
  #count number of wins and losses for each team
  awaygames = summarise(group_by(q,V2),num=n(),numwins=sum(!homewin))
  homegames = summarise(group_by(q,V4),num=n(),numwins=sum(homewin))
  totalgames = full_join(awaygames,homegames,by=c(V2="V4"))
  totalgames$num.x[is.na(totalgames$num.x)]=0
  totalgames$num.y[is.na(totalgames$num.y)]=0
  totalgames$numwins.x[is.na(totalgames$numwins.x)]=0
  totalgames$numwins.y[is.na(totalgames$numwins.y)]=0
  totalgames$wins = totalgames$numwins.x+totalgames$numwins.y
  totalgames$losses=totalgames$num.x+totalgames$num.y-totalgames$numwins.x-totalgames$numwins.y
  
  #create vector of opponents for each team  (opponents are identified by name)
  homespread = summarise(group_by(q,V4),o1=first(V2),o2=nth(V2,2),o3=nth(V2,3),o4=nth(V2,4),o5=nth(V2,5),o6=nth(V2,6),o7=nth(V2,7),o8=nth(V2,8),o9=nth(V2,9)) 
  awayspread = summarise(group_by(q,V2),o1=first(V4),o2=nth(V4,2),o3=nth(V4,3),o4=nth(V4,4),o5=nth(V4,5),o6=nth(V4,6),o7=nth(V4,7),o8=nth(V4,8),o9=nth(V4,9)) 
  totalspread = full_join(awayspread,homespread,by=c(V2="V4"))
  
  opps=list()
  for(i in 1:length(totalgames$V2)){
    tmp=as.character(totalspread[i,])
    tmp=tmp[-1]
    tmp=tmp[!is.na(tmp)]
    opps[[i]]=tmp
  }
  
  totalgames$opponents=opps
  
  
  
  C <- matrix(integer(length(totalgames$V2)^2),nrow=length(totalgames$V2))
  rownames(C)=totalgames$V2
  colnames(C)=totalgames$V2
  b <- numeric(length(totalgames$V2))
  for(i in 1:length(totalgames$V2)){
    C[i,i]=2+totalgames$wins[i]+totalgames$losses[i]
    for(j in totalgames$opponents[[i]]){
      C[i,j]=C[i,j]-1
    }
    b[i]=1+(totalgames$wins[i]-totalgames$losses[i])/2
  }
  r <- solve(C,b)
  totalgames <- cbind(totalgames,r)
  names(totalgames)[9] <- "rankscore"
  totalgames <- totalgames[order(-totalgames$rankscore),]
  
  solution = totalgames[,c(1,9)]
  rownames(solution)=NULL
  return(solution)