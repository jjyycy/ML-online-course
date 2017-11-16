RetsOnly = trainset[fullrow,29:147]
pcaout=princomp(RetsOnly)
summary(pcaout)
PCA1=pcaout$scores[,1]
fitmodel1=lm(Ret_PlusOne ~ Ret_4 + Ret_5 + Ret_6 + Ret_7 + Ret_8 + Ret_9 + 
               Ret_10 + Ret_12 + Ret_13 + Ret_14 + Ret_15 + Ret_16 + Ret_17 + 
               Ret_18 + Ret_19 + Ret_20 + Ret_21 + Ret_22 + Ret_23 + Ret_24 + 
               Ret_25 + Ret_26 + Ret_27 + Ret_28 + Ret_29 + Ret_31 + Ret_33 + 
               Ret_34 + Ret_36 + Ret_37 + Ret_38 + Ret_40 + Ret_41 + Ret_42 + 
               Ret_43 + Ret_44 + Ret_48 + Ret_49 + Ret_50 + Ret_53 + Ret_55 + 
               Ret_57 + Ret_58 + Ret_59 + Ret_60 + Ret_61 + Ret_62 + Ret_64 + 
               Ret_65 + Ret_66 + Ret_68 + Ret_69 + Ret_70 + Ret_71 + Ret_72 + 
               Ret_73 + Ret_74 + Ret_75 + Ret_77 + Ret_78 + Ret_79 + Ret_80 + 
               Ret_81 + Ret_82 + Ret_83 + Ret_84 + Ret_85 + Ret_86 + Ret_87 + 
               Ret_89 + Ret_90 + Ret_91 + Ret_92 + Ret_95 + Ret_99 + Ret_100 + 
               Ret_101 + Ret_102 + Ret_103 + Ret_104 + Ret_105 + Ret_107 + 
               Ret_109 + Ret_113 + Ret_114 + Ret_115 + Ret_116 + Ret_117 + 
               Ret_119 + Ret_120 + PCA1,data=subset(trainset,fullrow==TRUE))
cat("AIC of model in Exer2 =",AIC(fitmodel))
cat("AIC of new model =",AIC(fitmodel1))

fitmodel2=lm(Ret_PlusOne ~ Ret_4 + Ret_5 + Ret_6 + Ret_7 + Ret_8 + Ret_9 + 
               Ret_10 + Ret_12 + Ret_13 + Ret_14 + Ret_15 + Ret_16 + Ret_17 + 
               Ret_18 + Ret_19 + Ret_20 + Ret_21 + Ret_22 + Ret_23 + Ret_24 + 
               Ret_25 + Ret_26 + Ret_27 + Ret_28 + Ret_29 + Ret_31 + Ret_33 + 
               Ret_34 + Ret_36 + Ret_37 + Ret_38 + Ret_40 + Ret_41 + Ret_42 + 
               Ret_43 + Ret_44 + Ret_48 + Ret_49 + Ret_50 + Ret_53 + Ret_55 + 
               Ret_57 + Ret_58 + Ret_59 + Ret_60 + Ret_61 + Ret_62 + Ret_64 + 
               Ret_65 + Ret_66 + Ret_68 + Ret_69 + Ret_70 + Ret_71 + Ret_72 + 
               Ret_73 + Ret_74 + Ret_75 + Ret_77 + Ret_78 + Ret_79 + Ret_80 + 
               Ret_81 + Ret_82 + Ret_83 + Ret_84 + Ret_85 + Ret_86 + Ret_87 + 
               Ret_89 + Ret_90 + Ret_91 + Ret_92 + Ret_95 + Ret_99 + Ret_100 + 
               Ret_101 + Ret_102 + Ret_103 + Ret_104 + Ret_105 + Ret_107 + 
               Ret_109 + Ret_113 + Ret_114 + Ret_115 + Ret_116 + Ret_117 + 
               Ret_119 * Ret_120,data=subset(trainset,fullrow==TRUE))
cat("AIC of new model2 with interaction term =",AIC(fitmodel2))



