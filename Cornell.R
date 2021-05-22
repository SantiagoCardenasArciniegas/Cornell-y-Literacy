library(boot)
library(ggm)
library(ggplot2)
library(polycor)
library(Hmisc)
library(dplyr)
library(readxl)
library(devtools)
library(tidyverse)
library(lubridate)
library(ggridges)
library(wesanderson)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(hrbrthemes)
library(statsr)
library(stargazer)
library(psych)
library(corrplot)
library(corrr)
library(GGally)
library(ggcorrplot)
library(PerformanceAnalytics)
library(pander)
library(broom)
library(purrr)
library(kableExtra)
library(egg)

#library(rstatix)
#library(ggpubr)
webshot::install_phantomjs()



# Alistando los datos -----------------------------------------------------

#Working Directory

#setwd("~/Santiago/Clases Camila/Cornell_Tests")

#Creando carpetas para guardar los outputs

dir.create("Correlation_Matrices_html")
dir.create("Summary_Tables")
dir.create("Correlation_Plots")
dir.create("Regression_Tables")
dir.create("Graficas_Definitivas")
dir.create("./Graficas_Definitivas/SVG")
dir.create("./T_Tests")

#Cargando datos

Cornell_niveles <- read_excel("Cornell_niveles.xlsx")

Cornell_literacy <- read_excel("Correlations_CornellLiteracy.xlsx")


#Voy a empezar explorando la base Cornell_niveles

head(Cornell_niveles)

View(Cornell_niveles)

#Renombrando variables

names(Cornell_niveles)


Cornell_niveles<- Cornell_niveles %>% 
  rename(Induction_Pre=`Induction pre`,
         Induction_Post=`Induction Post`,
         Deduction_Pre=`Deduction Pre`,
         Deduction_Post=`Deduction Post`,
         Obs_Cred_Pre=`Obs Cred Pre`,
         Obs_Cred_Post=`Obs Cred Post`,
         Assumptions_Pre=`Assumptions Pre`,
         Assumptions_Post=`Assumptions post`)

#La misma base con los nuevos nombres (sin espacios)
names(Cornell_niveles)


#Para lograr meter todo en una sola gráfica necesitamos una variable
#Categórica que nos indique si el valor del test es pre o post.Eso
#me permite unificar los valores de una misma competencia en una sola columna. 
#para ello tendré que camiar de nuevo los nombres. El nombre de cada columna va
#a ser la competencia como tal. Sin el "pre y el post".



#Creando nueva variable categórica.(TiMe. Dos niveles (Pre y Post))
names(Cornell_niveles)

#Pre

Cornell_niveles_Pre <- Cornell_niveles %>% 
  select(Total_PreCornell,Induction_Pre,Deduction_Pre,
         Obs_Cred_Pre,Assumptions_Pre) %>% 
  mutate(Time=as.factor("Pre"))

  #Renaming

Cornell_niveles_Pre <- Cornell_niveles_Pre %>% 
    rename(
      Total_Cornell=Total_PreCornell, 
      Induction= Induction_Pre,    
      Deduction=Deduction_Pre,    
      Obs_Cred=Obs_Cred_Pre,   
      Assumptions=Assumptions_Pre  
    )


#Creando variable Skill

Cornell_niveles_Pre_Total <- Cornell_niveles_Pre %>% 
  select(Total_Cornell,Time) %>% 
  mutate(Skill=as.factor("Total")) %>% 
  rename(Score=Total_Cornell)

Cornell_niveles_Pre_Induction <- Cornell_niveles_Pre %>% 
  select(Induction,Time) %>% 
  mutate(Skill=as.factor("Induction")) %>% 
  rename(Score=Induction)

Cornell_niveles_Pre_Deduction <- Cornell_niveles_Pre %>% 
    select(Deduction,Time) %>% 
  mutate(Skill=as.factor("Deduction")) %>% 
  rename(Score=Deduction)

Cornell_niveles_Pre_Obs_Cred <- Cornell_niveles_Pre %>% 
  select(Obs_Cred,Time) %>% 
  mutate(Skill=as.factor("Obs_Cred")) %>% 
  rename(Score=Obs_Cred)

Cornell_niveles_Pre_Assumptions <- Cornell_niveles_Pre %>% 
  select(Assumptions,Time) %>% 
  mutate(Skill=as.factor("Assumptions")) %>% 
  rename(Score=Assumptions)

Base_Pre <- rbind(Cornell_niveles_Pre_Total,
                  Cornell_niveles_Pre_Induction,
                  Cornell_niveles_Pre_Deduction,
                  Cornell_niveles_Pre_Obs_Cred,
                  Cornell_niveles_Pre_Assumptions)



#Post

Cornell_niveles_Post <- Cornell_niveles %>% 
  select(Total_PostCornell,Induction_Post,Deduction_Post,
         Obs_Cred_Post,Assumptions_Post) %>% 
  mutate(Time=as.factor("Post"))


  #Renaming

  Cornell_niveles_Post<- Cornell_niveles_Post %>% 
  rename(
    Total_Cornell=Total_PostCornell, 
    Induction= Induction_Post,    
    Deduction=Deduction_Post,    
    Obs_Cred=Obs_Cred_Post,   
    Assumptions=Assumptions_Post  
    )

#Creando variable skill en Post
  
  Cornell_niveles_Post_Total <- Cornell_niveles_Post %>% 
    select(Total_Cornell,Time) %>% 
    mutate(Skill=as.factor("Total")) %>% 
    rename(Score=Total_Cornell)
  
  
  Cornell_niveles_Post_Induction <- Cornell_niveles_Post %>% 
    select(Induction,Time) %>% 
    mutate(Skill=as.factor("Induction")) %>% 
    rename(Score=Induction)
  
  Cornell_niveles_Post_Deduction <- Cornell_niveles_Post %>% 
    select(Deduction,Time) %>% 
    mutate(Skill=as.factor("Deduction")) %>% 
    rename(Score=Deduction)
  
  Cornell_niveles_Post_Obs_Cred <- Cornell_niveles_Post %>% 
    select(Obs_Cred,Time) %>% 
    mutate(Skill=as.factor("Obs_Cred")) %>% 
    rename(Score=Obs_Cred)
  
  Cornell_niveles_Post_Assumptions <- Cornell_niveles_Post %>% 
    select(Assumptions,Time) %>% 
    mutate(Skill=as.factor("Assumptions")) %>% 
    rename(Score=Assumptions)
  
  Base_Post <- rbind(Cornell_niveles_Post_Total,
                    Cornell_niveles_Post_Induction,
                    Cornell_niveles_Post_Deduction,
                    Cornell_niveles_Post_Obs_Cred,
                    Cornell_niveles_Post_Assumptions)

##Uniendo las dos bases con rbind
  
    #Reducida a 3 column_to_rownames

    Cornell_niveles_Pre_Post<- rbind(Base_Pre,Base_Post)

    #Base completa con skills en columnas distintas

    Extended_Cornell_niveles_Pre_Post <- rbind(Cornell_niveles_Pre,Cornell_niveles_Post)


# Gráficos ----------------------------------------------------------------


#Boxplot normal

  Cornell_niveles_Pre_Post %>% 
      ggplot(aes(y=Score,x=Skill,fill= Time))+
      geom_boxplot(alpha = 0.6)+
      scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
      labs(title="Cornell Scores",
           subtitle = "Pre vs Post",
           y="Critical thinking score",
           x="Skill")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),#Tamaños de letra
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
  #Export
    
  Plot1<- Cornell_niveles_Pre_Post %>% 
  ggplot(aes(y=Score,x=Skill,fill= Time))+
  geom_boxplot(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Cornell Scores",
       subtitle = "Pre vs Post",
       y="Critical thinking score",
       x="Skill")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

  ggsave(plot = Plot1, filename = "./Graficas_Definitivas/Boxplot_Cornell_scores.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")


  ggsave(plot = Plot1, filename = "./Graficas_Definitivas/SVG//Boxplot_Cornell_scores.svg", 
       width = 11, 
       height = 7,
       dpi = "retina")
  

#Boxplot con Geom Point
  
  Cornell_niveles_Pre_Post %>% 
    ggplot(aes(y=Score,x=Skill,fill= Time))+
    geom_boxplot(alpha = 0.6)+
    geom_point(alpha = 0.4)+ #Esta linea agrega los puntos sobre el boxplot
    scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
    labs(title="Cornell Scores",
         subtitle = "Pre vs Post",
         y="Critical thinking score",
         x="Skill")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13),#Tamaños de letra
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  #Export
  
  Plot2 <- Cornell_niveles_Pre_Post %>% 
ggplot(aes(y=Score,x=Skill,fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_point(alpha = 0.4)+ #Esta linea agrega los puntos sobre el boxplot
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Cornell Scores",
       subtitle = "Pre vs Post",
       y="Critical thinking score",
       x="Skill")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(plot = Plot2, filename = "./Graficas_Definitivas/Boxplot_Cornell_scores_GeomPoint.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")

ggsave(plot = Plot2, filename = "./Graficas_Definitivas/SVG/Boxplot_Cornell_scores_GeomPoint.svg", 
       width = 11, 
       height = 7,
       dpi = "retina")




#Boxplot con Jitter

Cornell_niveles_Pre_Post %>% 
  ggplot(aes(y=Score,x=Skill,fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_jitter(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Cornell Scores",
       subtitle = "Pre vs Post",
       y="Critical thinking score",
       x="Skill")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

  #Export

  Plot3<- Cornell_niveles_Pre_Post %>% 
  ggplot(aes(y=Score,x=Skill,fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_jitter(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Cornell Scores",
       subtitle = "Pre vs Post",
       y="Critical thinking score",
       x="Skill")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(plot = Plot3, filename = "./Graficas_Definitivas/Boxplot_Cornell_scores_Jitter.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")

ggsave(plot = Plot3, filename = "./Graficas_Definitivas/SVG//Boxplot_Cornell_scores_Jitter.svg", 
       width = 11, 
       height = 7,
       dpi = "retina")



#Barplot Medias Cornell (Fill con palleta Brewer)

Mean_Cornell_niveles_Pre_Post<-Cornell_niveles_Pre_Post %>% 
  group_by(Skill,Time) %>% 
  summarise(Mean_Cornell=mean(Score))


Mean_Cornell_niveles_Pre_Post %>% 
  ggplot(aes(x=factor(Skill),y=Mean_Cornell, fill = factor(Time))) +
  geom_col(position = "dodge")+
  scale_fill_brewer(palette = "Accent")+
  labs(title="Cornell test scores (Pre vs Post)", 
       subtitle= "By Skill",
       x="Skill",
       y = "Mean score",
       fill="Time")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


#Export

Barplot_Means_Cornell<-Mean_Cornell_niveles_Pre_Post %>% 
  ggplot(aes(x=factor(Skill),y=Mean_Cornell, fill = factor(Time))) +
  geom_col(position = "dodge")+
  scale_fill_brewer(palette = "Accent")+
  labs(title="Cornell test scores (Pre vs Post)", 
       subtitle= "By Skill",
       x="Skill",
       y = "Mean score",
       fill="Time")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

  ggsave(plot = Barplot_Means_Cornell, 
       filename = "./Graficas_Definitivas/Barplot_Means_Cornell.png", 
       width = 11, height = 7,
       type= "cairo",
       dpi = "retina")
  
  ggsave(plot = Barplot_Means_Cornell, 
         filename = "./Graficas_Definitivas/SVG/Barplot_Means_Cornell.svg", 
         width = 11, height = 7,
         dpi = "retina")
  
  

# T tests -----------------------------------------------------------------
  
  #Ttests
  
  Cornell_niveles_Pre_Post %>% 
    inference(y =Score , x =Time, statistic = "mean", type = "ht", 
              null = 0, alternative = "twosided", method = "theoretical")
  
  Cornell_niveles_Pre_Post %>% 
    inference(y =Score , x =Time, statistic = "mean", type = "ci", 
              null = 0, alternative = "twosided", method = "theoretical")
  
  

  #Export
   
  Ttest_total<-t.test(Cornell_niveles_Pre$Total_Cornell, Cornell_niveles_Post$Total_Cornell)
  
  tidy_Ttest_total <- tidy(Ttest_total)  
  
  tidy_Ttest_total %>% 
    dplyr::select(-method) %>% 
    rename(
      "Estimate"=estimate,
      "Mean_Pre"=estimate1,
      "Mean_Post"=estimate2,
      "T-statistic"= statistic,
      "P-value" = p.value,
      "Df"        = parameter,
      "Conf.high"= conf.high,
      "Conf.low" = conf.low
    ) %>%
    kable(
      format   = "html",
      caption  = "T-test for Total Cornell Score (Pre vs Post)",
      booktabs = TRUE,
      digits   = 2
    ) %>%
    kable_styling(
      bootstrap_options = "striped",
      full_width        = FALSE
    ) %>%
    row_spec(row = 0, color = "#000C35") %>% 
    kable_minimal() %>% 
    save_kable("./T_Tests/T_Test_Cornel_Score.png") 
  
  
  
  #Anova
  
  Anova <- aov(Score ~ Skill , data = Cornell_niveles_Pre_Post)

  summary(Anova)
  


# Correlaciones con Cornell Literacy --------------------------------------

#Cornelll Literacy
  
#Renaming
  Cornell_literacy <-  Cornell_literacy %>% 
    rename(Name =`Nom `)
  
sum(is.na(Cornell_literacy))

#La base tiene 28 Nas. La manera más facil de quitarlos es con  na.omit()
#pero estaría borrando datos innecesariamente si lo hago así

NaOmit_Cornell_literacy <- Cornell_literacy %>% 
  na.omit()


#Comparación tamaño de las bases

length(NaOmit_Cornell_literacy$Pre_MediaLiteracy)
length(Cornell_literacy$Pre_MediaLiteracy)


#Lo mejor acá, dado que se trata de Test Distintos es guardarlos
#en bases distintas y a cada una de las bases aplicarles na.omit()


Base_Pre_MediaLiteracy<-Cornell_literacy %>% 
  select(Name,Pre_MediaLiteracy,Pre_Cornell) %>% 
  na.omit()


Base_Pre_IREP<- Cornell_literacy %>% 
  select(Name,Pre_IREP,Pre_Cornell) %>% 
  na.omit()


Base_Pre_Otho3 <- Cornell_literacy %>% 
  select(Name,Pre_Otho3,Pre_Cornell) %>% 
  na.omit()


#Summary Statistics 

  #Base_Pre_MediaLiteracy

    stargazer(as.data.frame(Base_Pre_MediaLiteracy),
          type = "text",
          title = "Summary statistics Pre MediaLiteracy",
          digits = 2,
          style = "jpam", 
          out="./Summary_Tables/Base_Pre_MediaLiteracy.html")

  
  #Base_Pre_IREP
  
    stargazer(as.data.frame(Base_Pre_IREP),
            type = "text",
            title = "Summary statistics Pre IREP",
            digits = 2,
            style = "jpam", 
            out="./Summary_Tables/Base_Pre_IREP.html")
    
    
    
  #Base_Pre_Otho3
  
    
    stargazer(as.data.frame(Base_Pre_Otho3),
              type = "text",
              title = "Summary statistics Base Pre Otho3",
              digits = 2,
              style = "jpam", 
              out="./Summary_Tables/Base_Pre_Otho3.html")
    
    
    
#Scatterplots
    
  #Base_Pre_IREP
    
    Base_Pre_IREP %>% 
      ggplot(aes(y=Pre_Cornell,x=Pre_IREP))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs IREP")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
    #Export
    Scatter_Base_Pre_IREP<- Base_Pre_IREP %>% 
      ggplot(aes(y=Pre_Cornell,x=Pre_IREP))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs IREP")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
    ggsave(plot = Scatter_Base_Pre_IREP, 
           filename = "./Graficas_Definitivas/Scatter_Base_Pre_IREP.png", 
           width = 9, height = 7,
           type= "cairo",
           dpi = "retina")
    
    ggsave(plot = Scatter_Base_Pre_IREP, 
           filename = "./Graficas_Definitivas/SVG/Scatter_Base_Pre_IREP.svg", 
           width = 9, height = 7,
           dpi = "retina")

    
    
  #Base_Pre_MediaLiteracy
    
    Base_Pre_MediaLiteracy %>% 
      ggplot(aes(y=Pre_Cornell,x=Pre_MediaLiteracy))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs MediaLiteracy")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
  #Export
    
    Scatter_Base_Pre_MediaLiteracy<- Base_Pre_MediaLiteracy %>% 
      ggplot(aes(y=Pre_Cornell,x=Pre_MediaLiteracy))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs MediaLiteracy")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
    ggsave(plot = Scatter_Base_Pre_MediaLiteracy, 
           filename = "./Graficas_Definitivas/Scatter_Base_Pre_MediaLiteracy.png", 
           width = 9, height = 7,
           type= "cairo",
           dpi = "retina")
    
    ggsave(plot = Scatter_Base_Pre_MediaLiteracy, 
           filename = "./Graficas_Definitivas/SVG/Scatter_Base_Pre_MediaLiteracy.svg", 
           width = 9, height = 7,
           dpi = "retina")
    
    
  
  #Base_Pre_Otho3
    
    Base_Pre_Otho3 %>% 
      ggplot(aes(y=Pre_Cornell,x= Pre_Otho3))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs Otho3")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
    #Export
    
    Scatter_Base_Pre_Otho3 <- Base_Pre_Otho3 %>% 
      ggplot(aes(y=Pre_Cornell,x= Pre_Otho3))+
      geom_point()+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="Cornell vs Otho3")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
    
    
    ggsave(plot = Scatter_Base_Pre_Otho3, 
           filename = "./Graficas_Definitivas/Scatter_Base_Pre_Otho3.png", 
           width = 9, height = 7,
           type= "cairo",
           dpi = "retina")
    
    ggsave(plot = Scatter_Base_Pre_Otho3, 
           filename = "./Graficas_Definitivas/SVG/Scatter_Base_Pre_Otho3.svg", 
           width = 9, height = 7,
           dpi = "retina")
    
    
  #Los tres Scatter en un solo gráfico
    
    ggarrange(Scatter_Base_Pre_IREP,
              Scatter_Base_Pre_MediaLiteracy,
              Scatter_Base_Pre_Otho3,
              nrow = 1,ncol = 3)
    
    #Export
    
    SBS_Scatter_TestsVsCornell <- ggarrange(Scatter_Base_Pre_IREP,
                Scatter_Base_Pre_MediaLiteracy,
                Scatter_Base_Pre_Otho3,
                nrow = 1,ncol = 3)
    
    ggsave(plot = SBS_Scatter_TestsVsCornell, 
           filename = "./Graficas_Definitivas/SBS_Scatter_TestsVsCornell.png", 
           width = 16, height = 7,
           type= "cairo",
           dpi = "retina")
    
    
    ggsave(plot = SBS_Scatter_TestsVsCornell, 
           filename = "./Graficas_Definitivas/SVG/SBS_Scatter_TestsVsCornell.svg", 
           width = 16, height = 7,
           dpi = "retina")
    

    
  #Corplots
    
    #Usando Corplot
    
    ForCorplot_Cornell_literacy<-Cornell_literacy %>% 
      select(-Name)
    
    corrplot.mixed(cor(ForCorplot_Cornell_literacy,use = "pairwise.complete.obs"),
                   lower = "ellipse", 
                   upper = "number",
                   tl.col = "black",
                   title="Correlaciones Cornell Literacy")
    
    
    #Export
    
    png(height=1800, width=1800, file="./Correlation_Plots/Cornell_literacy.png", type = "cairo",res = 300)
    
    
    corrplot.mixed(cor(ForCorplot_Cornell_literacy,use = "pairwise.complete.obs"),
                   lower = "ellipse", 
                   upper = "number",
                   tl.col = "black",
                   title="Correlaciones Cornell Literacy")
    
    dev.off()
  
    
    
    #Usando ggcorrplot
    
    ggcorrplot(cor(ForCorplot_Cornell_literacy,use = "pairwise.complete.obs"),
               hc.order=TRUE, type='lower',lab = TRUE,
               title = "Correlaciones Well Being",
               ggtheme = ggplot2::theme_minimal(),
               colors = c("#F652A0","white","#36EEE0"))
    
    
    #Export
    
    ggcorrplot_ForCorplot_Cornell_literacy  <- ggcorrplot(cor(ForCorplot_Cornell_literacy,use = "pairwise.complete.obs"),
                          hc.order=TRUE, type='lower',lab = TRUE,
                          title = "Correlaciones Well Being",
                          ggtheme = ggplot2::theme_minimal(),
                          colors = c("#F652A0","white","#36EEE0"))
    
    ggsave(plot = ggcorrplot_ForCorplot_Cornell_literacy, filename = "./Correlation_Plots/ggcorrplot_ForCorplot_Cornell_literacy.png", 
           width = 9, 
           height = 7,
           type = "cairo",
           dpi = "retina")
    
    
#Regresiones Lineales
    #Cuando hago las tablas con Stargazer pongo "Text" en type
    #Para que la tabla me aparezca en el programa, pero dejo
    #el formato de out en ".html"
    
    
  #Base_Pre_IREP
    
    m1_Base_Pre_IREP<-lm( Pre_Cornell ~ Pre_IREP,
                             data = Base_Pre_IREP)

     #Export
    
     stargazer(m1_Base_Pre_IREP,
               ci = TRUE,
               type = "text",
               title="Pre Cornell ~ Pre IREP",
               align=TRUE,
               out = "Regression_Tables/m1_Base_Pre_IREP.html")
     
  #Base_Pre_MediaLiteracy
     
     m1_Base_Pre_MediaLiteracy<-lm( Pre_Cornell ~ Pre_MediaLiteracy,
                           data = Base_Pre_MediaLiteracy)
     
     
     #Export
     
     stargazer(m1_Base_Pre_MediaLiteracy,
               ci = TRUE,
               type = "text",
               title="Pre Cornell ~ Pre MediaLiteracy",
               align=TRUE,
               out = "Regression_Tables/m1_Base_Pre_MediaLiteracy.html")
     
    
  #Base_Pre_Otho3
     
     m1_Base_Pre_Otho3<-lm( Pre_Cornell ~ Pre_Otho3,
                                    data = Base_Pre_Otho3)

     #Export
     
     stargazer(m1_Base_Pre_Otho3,
               ci = TRUE,
               type = "text",
               title="Pre Cornell ~ Pre Otho3",
               align=TRUE,
               out = "Regression_Tables/m1_Base_Pre_Otho3.html")
     
     
  #Los tres modelos en una misma tabla
     
     #Export
     stargazer( m1_Base_Pre_IREP,
                m1_Base_Pre_MediaLiteracy,
                m1_Base_Pre_Otho3,
               ci = TRUE,
               type = "text",
               title="Pre Cornell ~ Literacy Tests",
               align=TRUE,
               out = "Regression_Tables/m1_Full_Table.html")
     
     
     
  