pre2002<-read.table("campy_pre2002.txt", header = TRUE,sep = "\t")

data_0205<-read.csv("campy_2002-2005.csv")

post2005<-read.csv("campy_2005-.csv")

##step1
pre2002_ny<-subset.data.frame(pre2002,!pre2002$SEKTION=="res")
##step2
pre2002_ny2<-subset.data.frame(pre2002,pre2002$AKTVNR=="5133")
##step3
pre2002_ny3<-subset.data.frame(pre2002_ny2,pre2002_ny2$CHR_NR>="10000")

data_0205_ny3<-subset.data.frame(data_0205,data_0205$Chrnr>="10000")

post2005_ny3<-subset.data.frame(post2005,post2005$Chrnr>="10000")

##step4
pre2002_ny4<-pre2002_ny3
pre2002_ny4$PRV_DATO<-as.Date(pre2002_ny4$PRV_DATO,format="%d%b%Y")

data_0205_ny4<-data_0205_ny3
data_0205_ny4$Prvdato<-as.Date(data_0205_ny4$Prvdato,format="%m/%d/%y")
#der er data fra f??r 2002 og efter 2005 med i datasettet

post2005_ny4<-post2005_ny3
post2005_ny4$Provedato<-as.Date(post2005_ny4$Provedato,format="%m/%d/%y")

##step5
pre2002_ny5<-pre2002_ny4[c("CHR_NR","EPINR","JNR","MATR","BAKTFUND","PRV_DATO",
                           "region")]
names(pre2002_ny5)<- c("chrnr","epinr","jnr","matr","resultat","prvdato","region")

data_0205_ny5<-data_0205_ny4[c("Chrnr","Epi.nr","Jnr","Materialeart","Resultat",
                               "Prvdato","region")]
names(data_0205_ny5)<- c("chrnr","epinr","jnr","matr","resultat","prvdato","region")

post2005_ny5<-post2005_ny4[c("Chrnr","Epinr","Jnr","Materialeart","Tolkning",
                             "Provedato","region")]
names(post2005_ny5)<- c("chrnr","epinr","jnr","matr","resultat","prvdato","region")

##step6
campy<-rbind(pre2002_ny5,data_0205_ny5,post2005_ny5)

##step7
campy7<-campy
campy7<-campy7[!is.na(campy$epinr),]

##step8
campy8<-campy7

levels(campy8$resultat) <- list("NEG"=levels(campy8$resultat)[c(1,10,12)], 
                                "POS"=levels(campy8$resultat)
                                [c(2,3,4,5,6,7,8,9,11,13)])

##step9
campy9<-campy8
campy9<-subset.data.frame(campy9,campy9$matr%in%c("Kloaksvaber","Svaberprøve",
                                                "766","772"))


#step10 if time permits....

#step11
campy11<-campy9
#campy11$ugenr<-strftime(as.POSIXct(campy9$prvdato),format = "%V")
campy11$ugenr<-as.numeric(ceiling(difftime(campy11$prvdato,
                                           strptime("19971229",format="%Y%m%d"),
                                           units="weeks")))

#step12
campy12<-subset.data.frame(campy11,campy11$prvdato>="1997-12-29")
#campy12 <- campy11

#step13
jnr1<-campy12$jnr[duplicated(campy12$jnr)]
jnr2<-unique(jnr1)
campy13<-subset.data.frame(campy12,!(campy12$jnr %in% jnr2))


#?????
#step14
campy14<-campy13
#step15

#campy15<-subset.data.frame(campy14,campy14$chrnr)

campy15<-campy14[campy14$chrnr %in% names(which(table(campy14$chrnr) >= 10)), ]

#step16
tot = list()
pos = list()

for (i in 1:max(campy15$ugenr)){
  tot[i]  <- length(which(campy15$ugenr == i))
  pos[i] <- length(which(campy15$ugenr == i & campy15$resultat == "POS"))
}
sum_camp <- data.frame(unlist(tot, recursive = TRUE, use.names = TRUE),
                       unlist(pos, recursive = TRUE, use.names = TRUE))
colnames(sum_camp) <- c("Total", "Positive")

sum(sum_camp$Total)

write.csv(campy15, file="final_data.csv")
write.csv(sum_camp, file="weekly_pos.csv")