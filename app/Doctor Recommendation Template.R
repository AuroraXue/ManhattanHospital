specialty_name<-c("CARDIAC SURGERY","GENERAL SURGERY","GYNECOLOGICAL ONCOLOGY","INFECTIOUS DISEASE","MEDICAL ONCOLOGY",
                  "NEPHROLOGY","OBSTETRICS/GYNECOLOGY","OPHTHALMOLOGY","SURGICAL ONCOLOGY","THORACIC SURGERY","VASCULAR SURGERY")
len=length(specialty_name)



# rank of doctor from Bellevue based on the score result

doctorScoreBellevue<-read.csv("score_BELLEVUE_HOSPITAL_CENTER.csv")
specialty_name<-names(table(doctorScoreBellevue$Primary.specialty))[order(table(doctorScoreBellevue$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreBellevue$Name<-paste(doctorScoreBellevue$First.Name,doctorScoreBellevue$Last.Name)
recommendDoctorBellevue<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorBellevue)<-c("Specialty","Name","Score")
recommendDoctorBellevue$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreBellevue[doctorScoreBellevue$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorBellevue$Name[recommendDoctorBellevue$Specialty==i]<-df$Name[1:2]
  recommendDoctorBellevue$Score[recommendDoctorBellevue$Specialty==i]<-df$score[1:2]
}

BellevueAverageScore<-c()
for(i in 1:len){
  BellevueAverageScore[i]=mean(doctorScoreBellevue$score[doctorScoreBellevue$Primary.specialty==specialty_name[i]])
}


# rank of doctor from Harlem based on the score result
doctorScoreHarlem<-read.csv("score_HARLEM_HOSPITAL_CENTER.csv")
specialty_name<-names(table(doctorScoreHarlem$Primary.specialty))[order(table(doctorScoreHarlem$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreHarlem$Name<-paste(doctorScoreHarlem$First.Name,doctorScoreHarlem$Last.Name)
recommendDoctorHarlem<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorHarlem)<-c("Specialty","Name","Score")
recommendDoctorHarlem$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreHarlem[doctorScoreHarlem$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorHarlem$Name[recommendDoctorHarlem$Specialty==i]<-df$Name[1:2]
  recommendDoctorHarlem$Score[recommendDoctorHarlem$Specialty==i]<-df$score[1:2]
}

HarlemAverageScore<-c()
for(i in 1:len){
  HarlemAverageScore[i]=mean(doctorScoreHarlem$score[doctorScoreHarlem$Primary.specialty==specialty_name[i]])
}

# rank of doctor from Special Surgery based on the score result
doctorScoreSpecial<-read.csv("score_HOSPITAL_FOR_SPECIAL_SURGERY.csv")
specialty_name<-names(table(doctorScoreSpecial$Primary.specialty))[order(table(doctorScoreSpecial$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreSpecial$Name<-paste(doctorScoreSpecial$First.Name,doctorScoreSpecial$Last.Name)
recommendDoctorSpecial<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorSpecial)<-c("Specialty","Name","Score")
recommendDoctorSpecial$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreSpecial[doctorScoreSpecial$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorSpecial$Name[recommendDoctorSpecial$Specialty==i]<-df$Name[1:2]
  recommendDoctorSpecial$Score[recommendDoctorSpecial$Specialty==i]<-df$score[1:2]
}

Special_HospitalAverageScore<-c()
for(i in 1:len){
  Special_HospitalAverageScore[i]=mean(doctorScoreSpecial$score[doctorScoreSpecial$Primary.specialty==specialty_name[i]])
}



# rank of doctor from NYU based on the score result
doctorScoreNYU<-read.csv("score_ikNYU_HOSPITALS_CENTER.csv")
specialty_name<-names(table(doctorScoreNYU$Primary.specialty))[order(table(doctorScoreNYU$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreNYU$Name<-paste(doctorScoreNYU$First.Name,doctorScoreNYU$Last.Name)
recommendDoctorNYU<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorNYU)<-c("Specialty","Name","Score")
recommendDoctorNYU$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreNYU[doctorScoreNYU$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorNYU$Name[recommendDoctorNYU$Specialty==i]<-df$Name[1:2]
  recommendDoctorNYU$Score[recommendDoctorNYU$Specialty==i]<-df$score[1:2]
}

NYUAverageScore<-c()
for(i in 1:len){
  NYUAverageScore[i]=mean(doctorScoreNYU$score[doctorScoreNYU$Primary.specialty==specialty_name[i]])
}


# rank of doctor from LENOX based on the score result
doctorScoreLENOX<-read.csv("score_LENOX_HILL_HOSPITAL.csv")
specialty_name<-names(table(doctorScoreLENOX$Primary.specialty))[order(table(doctorScoreLENOX$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreLENOX$Name<-paste(doctorScoreLENOX$First.Name,doctorScoreLENOX$Last.Name)
recommendDoctorLENOX<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorLENOX)<-c("Specialty","Name","Score")
recommendDoctorLENOX$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreLENOX[doctorScoreLENOX$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorLENOX$Name[recommendDoctorLENOX$Specialty==i]<-df$Name[1:2]
  recommendDoctorLENOX$Score[recommendDoctorLENOX$Specialty==i]<-df$score[1:2]
}

LENOXAverageScore<-c()
for(i in 1:len){
  LENOXAverageScore[i]=mean(doctorScoreLENOX$score[doctorScoreLENOX$Primary.specialty==specialty_name[i]])
}

# rank of doctor from METRO based on the score result
doctorScoreMETRO<-read.csv("score_METROPOLITAN_HOSPITAL_CENTER.csv")
specialty_name<-names(table(doctorScoreMETRO$Primary.specialty))[order(table(doctorScoreMETRO$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreMETRO$Name<-paste(doctorScoreMETRO$First.Name,doctorScoreMETRO$Last.Name)
recommendDoctorMETRO<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorMETRO)<-c("Specialty","Name","Score")
recommendDoctorMETRO$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreMETRO[doctorScoreMETRO$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorMETRO$Name[recommendDoctorMETRO$Specialty==i]<-df$Name[1:2]
  recommendDoctorMETRO$Score[recommendDoctorMETRO$Specialty==i]<-df$score[1:2]
}

METROAverageScore<-c()
for(i in 1:len){
  METROAverageScore[i]=mean(doctorScoreMETRO$score[doctorScoreMETRO$Primary.specialty==specialty_name[i]])
}

# rank of doctor from Mount_Beth based on the score result
doctorScoreMount_Beth<-read.csv("score_MOUNT_SINAI_BETH_ISRAEL_PETRIE_CAMPUS.csv")
specialty_name<-names(table(doctorScoreMount_Beth$Primary.specialty))[order(table(doctorScoreMount_Beth$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreMount_Beth$Name<-paste(doctorScoreMount_Beth$First.Name,doctorScoreMount_Beth$Last.Name)
recommendDoctorMount_Beth<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorMount_Beth)<-c("Specialty","Name","Score")
recommendDoctorMount_Beth$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreMount_Beth[doctorScoreMount_Beth$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorMount_Beth$Name[recommendDoctorMount_Beth$Specialty==i]<-df$Name[1:2]
  recommendDoctorMount_Beth$Score[recommendDoctorMount_Beth$Specialty==i]<-df$score[1:2]
}

Mount_BethAverageScore<-c()
for(i in 1:len){
  Mount_BethAverageScore[i]=mean(doctorScoreMETRO$score[doctorScoreMETRO$Primary.specialty==specialty_name[i]])
}

# rank of doctor from Mount Sinai based on the score result
doctorScoreMount<-read.csv("score_MOUNT_SINAI_HOSPITAL.csv")
specialty_name<-names(table(doctorScoreMount$Primary.specialty))[order(table(doctorScoreMount$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreMount$Name<-paste(doctorScoreMount$First.Name,doctorScoreMount$Last.Name)
recommendDoctorMount<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorMount)<-c("Specialty","Name","Score")
recommendDoctorMount$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreMount[doctorScoreMount$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorMount$Name[recommendDoctorMount$Specialty==i]<-df$Name[1:2]
  recommendDoctorMount$Score[recommendDoctorMount$Specialty==i]<-df$score[1:2]
}

MountAverageScore<-c()
for(i in 1:len){
  MountAverageScore[i]=mean(doctorScoreMETRO$score[doctorScoreMETRO$Primary.specialty==specialty_name[i]])
}


# rank of doctor from NEW YORK EYES AND EAR based on the score result
doctorScoreNYEE<-read.csv("score_N_Y_EYE_AND_EAR_INFIRMARY.csv")
specialty_name<-names(table(doctorScoreNYEE$Primary.specialty))[order(table(doctorScoreNYEE$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreNYEE$Name<-paste(doctorScoreNYEE$First.Name,doctorScoreNYEE$Last.Name)
recommendDoctorNYEE<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorNYEE)<-c("Specialty","Name","Score")
recommendDoctorNYEE$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreNYEE[doctorScoreNYEE$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorNYEE$Name[recommendDoctorNYEE$Specialty==i]<-df$Name[1:2]
  recommendDoctorNYEE$Score[recommendDoctorNYEE$Specialty==i]<-df$score[1:2]
}

NYEEAverageScore<-c()
for(i in 1:len){
  NYEEAverageScore[i]=mean(doctorScoreNYEE$score[doctorScoreNYEE$Primary.specialty==specialty_name[i]])
}

# rank of doctor from NEW YORK PRESBYTERIAN based on the score result
doctorScorePRES<-read.csv("score_NEW_YORK_PRESBYTERIAN_HOSPITAL.csv")
specialty_name<-names(table(doctorScorePRES$Primary.specialty))[order(table(doctorScorePRES$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScorePRES$Name<-paste(doctorScorePRES$First.Name,doctorScorePRES$Last.Name)
recommendDoctorPRES<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorPRES)<-c("Specialty","Name","Score")
recommendDoctorPRES$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScorePRES[doctorScorePRES$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorPRES$Name[recommendDoctorPRES$Specialty==i]<-df$Name[1:2]
  recommendDoctorPRES$Score[recommendDoctorPRES$Specialty==i]<-df$score[1:2]
}

PRESAverageScore<-c()
for(i in 1:len){
  PRESAverageScore[i]=mean(doctorScorePRES$score[doctorScorePRES$Primary.specialty==specialty_name[i]])
}

# rank of doctor from ROCKEFELLER based on the score result
doctorScoreROCK<-read.csv("score_NEW_YORK_PRESBYTERIAN_HOSPITAL.csv")
specialty_name<-names(table(doctorScoreROCK$Primary.specialty))[order(table(doctorScoreROCK$Primary.specialty),decreasing = TRUE)[1:10]]
len=length(specialty_name)
doctorScoreROCK$Name<-paste(doctorScoreROCK$First.Name,doctorScoreROCK$Last.Name)
recommendDoctorROCK<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorROCK)<-c("Specialty","Name","Score")
recommendDoctorROCK$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreROCK[doctorScoreROCK$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE),]
  recommendDoctorROCK$Name[recommendDoctorROCK$Specialty==i]<-df$Name[1:2]
  recommendDoctorROCK$Score[recommendDoctorROCK$Specialty==i]<-df$score[1:2]
}

ROCKAverageScore<-c()
for(i in 1:len){
  ROCKAverageScore[i]=mean(doctorScoreROCK$score[doctorScoreROCK$Primary.specialty==specialty_name[i]])
}
