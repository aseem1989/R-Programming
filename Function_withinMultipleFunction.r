 m5ModelSpecs <-function(Inputdata,ValCutOff,M5Training,M5ApplicationData,Lebel,Biggerclass,SubsettingFeature){ 
    
    #######################################################################################################################
    ############################# Function for Creating Specs and Storing #################################################
    #######################################################################################################################
		  M5ModelBuild<-function(M5Training,M5ApplicationData,Lebel,SubSettingKey){
		  	  
		  Test<-M5ApplicationData
	      ModelCreateDate<-as.character("2014-12-03:23:59:59")  # Model Create Date
		  ModelM5<-list()
		  m5ModelsTemp<-list()
		  m5ModelsReset<-list()
		  m5ModelApply<-list()
		  PredictOutputFin<-NULL
		  PredictOutputTemp<-NULL
		  #write.table(NULL, file=paste0(FilePath,PredFlatFileOutPut),append=FALSE, sep="|", col.names = TRUE, row.names = FALSE, quote = FALSE)
		  m5Models<-data.frame(PredictOutput=NA, Model=NA,SubSettingKey=NA,i2=0)  
		  m5Models$Model<-list(NA)
		  m5Models$PredictOutput<-list(NA)
		  m5ModelsReset<-m5Models
		  m5ModelsTemp<-m5ModelsReset 
		  #close(channel)
		  print(SubSettingKey)
		  print(i2)
		  #Apply Model and Defining Response 
		  print("Build Model Stage")
		  cTune <- train(x = M5Training[,-1 * which(colnames(M5Training)==Lebel)], y = M5Training[,1 * which(colnames(M5Training)==Lebel)],"cubist",tuneGrid = expand.grid(.committees = c(1, 10, 50, 100),.neighbors = c(0, 1, 5, 9)),trControl = trainControl(method = "cv"))
		  Bestcommittees<-cTune$bestTune[1]
		  Bestneighbors<-cTune$bestTune[2]
		  ModelM5[[1]]<-cubist(x=M5Training[,-1 * which(colnames(M5Training)==Lebel)],y = M5Training[,1 * which(colnames(M5Training)==Lebel)],committees=Bestcommittees,neighbors=Bestneighbors)
		  print("Store Model Stage")
		  m5ModelsTemp$Model[1]=ModelM5[1]
		  print("Move Model to DF Stage")
		  m5ModelsTemp$SubSettingKey<- SubSettingKey
		  m5ModelsTemp$i2<- i2
		  m5ModelsTemp$PredictOutput<-list(NA)
		  print("Prediction Stage")
		  PredictedResults<-data.frame(Test,Prediction=predict(ModelM5[[1]], M5ApplicationData[, -1 * which(colnames(M5ApplicationData)==Lebel)]))  
		  PredictOutputFin<-rbind(PredictOutputFin,PredictedResults)
		  #write.table(PredictOutputFin, file=paste0(FilePath,PredFlatFileOutPut),append=TRUE, sep="|", col.names = TRUE, row.names = FALSE, quote = FALSE)
		  return(m5ModelsTemp)
		}
		
	  #Loop for creating M5 Model Specs 
		ListOfmodelClass<-as.character(unique(M5Training[,1 * which(colnames(M5Training)==Biggerclass)]))
		for (i in 1:length(ListOfmodelClass))
		{
				  #for (i in 1:2) {
				  tryCatch(
			{ 
			  #ModelFrameWrk <-"m5"
			  SubSettingKey <- ListOfmodelClass[i]  
			  i2<-i
			
			  ModelFunctOutPut<-M5ModelBuild(M5Training,M5ApplicationData,Lebel,SubSettingKey)  # Call modeling function specific to respective ModelingFrameWorkFramework  
			{
				m5Models<- if (i2==1)  { 
				  (ModelFunctOutPut) 
				}   else     { 
				  rbind(m5Models,ModelFunctOutPut)
				} 
			  }
			#m5Models$PredictOutput<-m5Models$ModelFrameWrk<-m5Models$TrainValCutOffMonth<-m5Models$i2<-m5Models$TrainValCutOffMont<-NULL
			save(m5Models,file=paste0(FilePath,"m5Models.RData"))  
			},
			# error= {
			#     print(cat("Error in",i2))
			#     next i2
			# },
			finally=NA )
		}
#######################################################################################################################
############################# Function for Creating Specs and Storing Ends Here#########################################
#######################################################################################################################

######################################################################################################################
############################# Applying m5 Specs to Test Data sets ######################################################
######################################################################################################################

		m5FunctionMonthly <-function(Inputdata,SubSettingKey1,ValCutOff,i2)
				{
				  # #Applying m5 Specs to Test Data set
				  state<-as.character(head(M5ApplicationData$R_GROUP[M5ApplicationData$REGION==SubSettingKey1],n=1) )
				  stateRule<-as.character(state)
				  ModelApply<-m5Models$Model[m5Models$SubSettingKey==stateRule][[1]]
				  M5ApplicationData1<-data.frame(Inputdata[(Inputdata$REGION==SubSettingKey1) & (Inputdata$YEAR>=ValCutOff),]) 
				  PredictedResults<-data.frame(M5ApplicationData1,Prediction=predict(ModelApply, M5ApplicationData1[, -1 * which(colnames(M5ApplicationData1)==Lebel)]))
				  return(PredictedResults)
				}

        ##Loop  for applying M5 specs on Test data

		nMaster<-length(unique(Inputdata[,1*which(colnames(Inputdata)==SubsettingFeature)]))

		for (i in 1:nMaster) 
				{
				gc()
									   #for (i in 1:1) {  
				tryCatch(
				{ SubSettingKey1 <- as.character(unique(Inputdata[,1*which(colnames(Inputdata)==SubsettingFeature)]))[i]
				  
				  i2<-i
				  
				  ModelFunctOutPut<-m5FunctionMonthly(Inputdata,SubSettingKey1,ValCutOff,i2)  # Call modeling function specific to respective ModelingFrameWorkFramework  
				  if(i==1) {FinalResults <- ModelFunctOutPut} else  {FinalResults<-rbind(FinalResults, ModelFunctOutPut)}  
				}, 
				error = function(err) 
				{
				  
				  print(paste("ERROR IN:  ",SubSettingKey1,"Error Details:  ",err))
				  
				},
				finally = {print(paste("End of Iteration:  ",i2))}
						)
				}
				#Unload Training data from m5 model specs 
				m5Models$Model[[1]][1] <-NULL
				return(FinalResults)
} 

#Calling the function 
Results<-m5ModelSpecs(Inputdata,ValCutOff,M5Training,M5ApplicationData,Lebel,Biggerclass,SubsettingFeature)
View(results)

