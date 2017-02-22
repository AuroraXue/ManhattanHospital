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


# rank of doctor from Special Surgery based on the score result
doctorScoreSpecial<-read.csv("score_HOSPITAL_FOR_SPECIAL_SURGERY.csv")
doctorScoreSpecial$Name<-paste(doctorScoreSpecial$First.Name,doctorScoreSpecial$Last.Name)
recommendDoctorSpecial<-as.data.frame(matrix(NA,nrow = len*2,ncol = 3))
colnames(recommendDoctorSpecial)<-c("Specialty","Name","Score")
recommendDoctorSpecial$Specialty<-rep(specialty_name,each=2)
for (i in specialty_name){
  df=doctorScoreSpecial[doctorScoreSpecial$Primary.specialty==i,]
  df=df[order(df$score,decreasing = TRUE,na.last = TRUE),]
  recommendDoctorSpecial$Name[recommendDoctorSpecial$Specialty==i]<-df$Name[1:2]
  recommendDoctorSpecial$Score[recommendDoctorSpecial$Specialty==i]<-df$score[1:2]
}


SpecialAverageScore<-c()
for(i in 1:len){
  SpecialAverageScore[i]=mean(doctorScoreSpecial$score[doctorScoreHarlem$Primary.specialty==specialty_name[i]])
}








