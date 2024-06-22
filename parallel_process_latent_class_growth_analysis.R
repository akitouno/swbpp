library(tidyverse)
library(MplusAutomation)

d0 <- read.csv('data0.csv', encoding='utf-8')
d <- d0[c('id', 'wave', 'sdqtd', 'who5')]

df_ps <- d[, c('id', 'wave', 'sdqtd')]
df_psw <- pivot_wider(df_ps, names_from=wave, values_from=sdqtd)
df_psw_th2 <- df_psw[rowSums(is.na(df_psw))<=2, ]
df_psl_th2 <- pivot_longer(df_psw_th2, cols=c('1', '2', '3', '4'), names_to='wave', values_to='sdqtd')
df_psl_th2$sdqtd_std <- as.numeric(scale(df_psl_th2$sdqtd))

df_w <- d[, c('id', 'wave', 'who5')]
df_ww <- pivot_wider(df_w, names_from=wave, values_from=who5)
df_ww_th2 <- df_ww[rowSums(is.na(df_ww))<=2, ]
df_wwl_th2 <- pivot_longer(df_ww_th2, cols=c('1', '2', '3', '4'), names_to='wave', values_to='who5')
df_wwl_th2$who5_std <- as.numeric(scale(df_wwl_th2$who5))

df_long <- merge(df_psl_th2, df_wwl_th2, by=c('id', 'wave'), how='innner')
df_long <- df_long[c('id', 'wave', 'sdqtd_std', 'who5_std')]
df <- pivot_wider(df_long, names_from='wave', values_from=c('sdqtd_std', 'who5_std'))
colnames(df) <- c('id', 'sdqtd1', 'sdqtd2', 'sdqtd3', 'sdqtd4', 'who1', 'who2', 'who3', 'who4')


#Linear models

pplcga1 <- mplusObject(
  TITLE='pplcga1;',
  VARIABLE='
  classes=c(1);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;
    i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;
     i1@0; s1@0; i2@0; s2@0;
    %c#1%
   [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga1.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga1 <- mplusModeler(pplcga1, modelout='pplcga1.inp', run=1L)


pplcga2 <- mplusObject(
  TITLE='pplcga2;',
  VARIABLE='
  classes=c(2);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;
    i1@0; s1@0; i2@0; s2@0;
    %c#1%
    [i1 s1 i2 s2];
    %c#2%
    [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga2.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga2 <- mplusModeler(pplcga2, modelout='pplcga2.inp', run=1L)

pplcga3 <- mplusObject(
  TITLE='pplcga3;',
  VARIABLE='
  classes=c(3);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; i2@0; s2@0;
    %c#1%
    [i1 s1 i2 s2];
    %c#2%
    [i1 s1 i2 s2];
    %c#3%
    [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga3.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga3 <- mplusModeler(pplcga3, modelout='pplcga3.inp', run=1L)

pplcga4 <- mplusObject(
  TITLE='pplcga4;',
  VARIABLE='
  classes=c(4);',
  ANALYSIS='
  type=mixture;

  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; i2@0; s2@0;
    
    %c#1%
    [i1 s1 i2 s2];
    %c#2%
    [i1 s1 i2 s2];
    %c#3%
    [i1 s1 i2 s2];
    %c#4%
    [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga4.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga4 <- mplusModeler(pplcga4, modelout='pplcga4.inp', run=1L)

pplcga5 <- mplusObject(
  TITLE='pplcga5;',
  VARIABLE='
  classes=c(5);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; i2@0; s2@0;
    %c#1%
    [i1 s1 i2 s2];
    %c#2%
    [i1 s1 i2 s2];
    %c#3%
    [i1 s1 i2 s2];
    %c#4%
    [i1 s1 i2 s2];    
    %c#5%
    [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga5.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga5 <- mplusModeler(pplcga5, modelout='pplcga5.inp', run=1L)

pplcga6 <- mplusObject(
  TITLE='pplcga6;',
  VARIABLE='
  classes=c(6);',
  ANALYSIS='
  type=mixture;

  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 | who1@0 who2@1 who3@2 who4@3;i2 s2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; i2@0; s2@0;
    %c#1%
    [i1 s1 i2 s2];
    %c#2%
    [i1 s1 i2 s2];
    %c#3%
    [i1 s1 i2 s2];
    %c#4%
    [i1 s1 i2 s2];
    %c#5%
    [i1 s1 i2 s2];
    %c#6%
    [i1 s1 i2 s2];',
  SAVEDATA='file=pplcga6.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga6 <- mplusModeler(pplcga6, modelout='pplcga6.inp', run=1L)


# Quadratic models

pplcga1_q <- mplusObject(
  TITLE='pplcga1_q;',
  VARIABLE='
  classes=c(1);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;
     i1@0; s1@0; q1@0; i2@0; s2@0; q2@0;
    %c#1%
   [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga1_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga1_q <- mplusModeler(pplcga1_q, modelout='pplcga1_q.inp', run=1L)


pplcga2_q <- mplusObject(
  TITLE='pplcga2_q;',
  VARIABLE='
  classes=c(2);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;
    i1@0; s1@0; q1@0; i2@0; s2@0; q2@0;
    %c#1%
    [i1 s1 q1 i2 s2 q2];
    %c#2%
    [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga2_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga2_q <- mplusModeler(pplcga2_q, modelout='pplcga2_q.inp', run=1L)

pplcga3_q <- mplusObject(
  TITLE='pplcga3_q;',
  VARIABLE='
  classes=c(3);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; q1@0; i2@0; s2@0; q2@0;
    %c#1%
    [i1 s1 q1 i2 s2 q2];
    %c#2%
    [i1 s1 q1 i2 s2 q2];
    %c#3%
    [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga3_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga3_q <- mplusModeler(pplcga3_q, modelout='pplcga3_q.inp', run=1L)

pplcga4_q <- mplusObject(
  TITLE='pplcga4_q;',
  VARIABLE='
  classes=c(4);',
  ANALYSIS='
  type=mixture;

  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; q1@0; i2@0; s2@0; q2@0;
    
    %c#1%
    [i1 s1 q1 i2 s2 q2];
    %c#2%
    [i1 s1 q1 i2 s2 q2];
    %c#3%
    [i1 s1 q1 i2 s2 q2];
    %c#4%
    [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga4_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga4_q <- mplusModeler(pplcga4_q, modelout='pplcga4_q.inp', run=1L)

pplcga5_q <- mplusObject(
  TITLE='pplcga5_q;',
  VARIABLE='
  classes=c(5);',
  ANALYSIS='
  type=mixture;
  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

    i1@0; s1@0; q1@0; i2@0; s2@0; q2@0;
    %c#1%
    [i1 s1 q1 i2 s2 q2];
    %c#2%
    [i1 s1 q1 i2 s2 q2];
    %c#3%
    [i1 s1 q1 i2 s2 q2];
    %c#4%
    [i1 s1 q1 i2 s2 q2];    
    %c#5%
    [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga5_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga5_q <- mplusModeler(pplcga5_q, modelout='pplcga5_q.inp', run=1L)

pplcga6_q <- mplusObject(
  TITLE='pplcga6_q;',
  VARIABLE='
  classes=c(6);',
  ANALYSIS='
  type=mixture;

  starts=1000 250;
  estimator=MLR',
  MODEL=
    '%overall%
    i1 s1 q1 | who1@0 who2@1 who3@2 who4@3;i2 s2 q2 | sdqtd1@0 sdqtd2@1 sdqtd3@2 sdqtd4@3;

     i1@0;s1@0;q1@0; 
    %c#1%
    [i1 s1 q1 i2 s2 q2];
    %c#2%
    [i1 s1 q1 i2 s2 q2];
    %c#3%
    [i1 s1 q1 i2 s2 q2];
    %c#4%
    [i1 s1 q1 i2 s2 q2];
    %c#5%
    [i1 s1 q1 i2 s2 q2];
    %c#6%
    [i1 s1 q1 i2 s2 q2];',
  SAVEDATA='file=pplcga6_q.dat;
      save=cprobabilities;',
  OUTPUT='SAMPSTAT CINTERVAL ENTROPY RESIDUAL TECH1 TECH4  TECH11;',
  rdata=df)

pplcga6_q <- mplusModeler(pplcga6_q, modelout='pplcga6_q.inp', run=1L)




