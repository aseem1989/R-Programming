#You tube Linked https://www.youtube.com/watch?v=j1V2McKbkLo

#intit
libs<-c("tm","plyr","class")
lapply(libs,require,character.only=TRUE)
#set options
options(stringsASFacter=FALSE)
#set parameter
candidates<-c("romney","obama")
pathname<-"D:/Modeling/SpeechsRecognision"
#clean text
  cleanCorpus<-function(corpus){

    corpus.tmp<-tm_map(corpus,removePunction)
    corpus.tmp<-tm_map(corpus.tmp,stripwhitespace)
    corpus.tmp<-tm_map(corpus.tmp,tolower)
    corpus.tmp<-tm_map(corpus.tmp,removewords,stopwords("english"))
    return(corpus.tmp)
  }

#Build TDM

generateTDM<-function(cand,path){
  s.dir<-sprintf("%s/%s",path,cand)
  s.cor<-corpus(DirSource(directory=s.dir,encoding="ANSI"))
  s.cor.cl<-cleanCorpus(s.cor)
  s.tdm<-TermDocumentMatrix(s.cor.cl)
  s.tdm<-removeSparseTerms(s.tdm,0.7)
  results<-list(name=cand,tdm=s.tdm)
}

tdm<-lapply(candidates,generateTDM,path=pathname)
#Attached name
bindCandidateTDM<-function(tdm){
  s.mat<-(dta.matrix(tdm[["tdm"]]))
  s.df<-as.data.frame(s.mat,stringsASFacter=FALSE)
  a.df<-cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)]<-"targetcandidate"
  return(s.df)
}
candTDM<-lapply(tdm,bindCandidateToTDM)

#Stack
tdm.stack<-do.cal(rbind.fill,candTDM)
tdm.stack[!is.na(tdm.stack)]<-0

#hold-out sample 
train.idx<-sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)=0.7))
test.idx<-(1:mrow(tdm.stack))[-train.idx]
#Models-KNN
tdm.cand<-tdm.stack[,"targetcandidate"]
tdm.stack.n<-tdm.stack[,!colnames(tdm.stack) %in%"targetcandidate"]
knn.pred<-knn(tdm.stack.nl[train.idx,],tdm.stack.nl[test.idx,],tdm.cand[train.idx])
                       
                       
#Accuracy
conf.mat<-table("Predictions"=knn.pres,Actual=tdm.cand[test.idx])
(accurary<-sum(diag(conf.mat))/length(test.idx)*100)
                       