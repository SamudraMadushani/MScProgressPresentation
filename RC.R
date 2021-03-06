#Remove codes from PPT

# Feature-based visualization 

```{r ,echo=FALSE, out.width = '100%', out.height = '100%',fig.height=8, fig.width=16}
library(broom)
library(ggrepel)
A<-DataDengueTEST %>%fabletools::aggregate_key(Province/Districts,Counts=sum(Counts))
DX<-A%>% features(Counts, feature_set(pkgs = "feasts"))
DX$Districts<-c("Kandy","Matale","NuwaraEliya","Central","Ampara","Batticaloa","Kalmune","Trincomalee",
                "Eastern","Anuradhapura","Polonnaruwa","NorthCentral","Kurunagala","Puttalam",    
                "NorthWestern","Jaffna","Kilinochchi","Mannar","Mullaitivu","Vavuniya","Northern",
                "Kegalle","Ratnapura","Sabaragamuwa","Galle","Hambanthota","Matara","Southern",
                "Badulla","Monaragala","Uva","Colombo","Gampaha","Kalutara","Western","Sri Lanka")

names(DX)[1] <- 'Level'
DX$Level<-c("District","District","District","Province","District","District","District","District",
            "Province","District","District","Province","District","District",    
            "Province","District","District","District","District","District","Province",
            "District","District","Province","District","District","District","Province",
            "District","District","Province","District","District","District","Province","Country")

pcsW <- DX %>%
  dplyr::select(-c(1,2,31,33,35,37)) %>%
  prcomp(scale = TRUE) %>%
  augment(DX)


FBVZW<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Level, label=Districts)) +
  geom_point() +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2")

FBVZW<-FBVZW+ theme(legend.position = "bottom",legend.box = "vertical")+scale_color_manual(values = c("#e41a1c","#6a3d9a","#ff7f00"))

FBVZW1<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = trend_strength, label=Districts)) +
  geom_point(aes(size=trend_strength)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Trend strength")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW1<-FBVZW1+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

(FBVZW|FBVZW1)

```


---
  
  # Feature-based visualization 
  
  ```{r ,echo=FALSE, out.width = '100%', out.height = '100%',fig.height=8, fig.width=16}

FBVZW2<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = seasonal_strength_year, label=Districts)) +
  geom_point(aes(size=seasonal_strength_year)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Seasonal strength")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW2<-FBVZW2+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

FBVZW3<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = spectral_entropy, label=Districts)) +
  geom_point(aes(size=spectral_entropy)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Entropy")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW3<-FBVZW3+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

(FBVZW2|FBVZW3)

```

---
  
  # Feature-based visualization 
  
  ```{r ,echo=FALSE, out.width = '100%', out.height = '100%',fig.height=8, fig.width=16}

FBVZW4<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = acf1, label=Districts)) +
  geom_point(aes(size=acf1)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "acf1")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW4<-FBVZW4+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

FBVZW5<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = n_crossing_points, label=Districts)) +
  geom_point(aes(size=n_crossing_points)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "crossing_points")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW5<-FBVZW5+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

(FBVZW4|FBVZW5)

```



# Visualization of model performance-Weekly series

```{r ,echo=FALSE}
library(readxl)
library(broom)
Weekly_20AD<- read_excel("C:/Users/USER/Desktop/Book1.xlsx")
Weekly_20AD<-Weekly_20AD[,-c(1)]

pcsWk20AD <- Weekly_20AD %>%
  dplyr::select(-c(1,2)) %>%
  prcomp(scale = TRUE) %>%
  augment(Weekly_20AD)

Method<-c("HS","AVG","HS","ETS","HS","SNAIVE",rep("AVG",3),rep("HS",3),"AVG",rep("HS",2),
          rep("AVG",3),rep("HS",2),rep("AVG",2),rep("HS",1),rep("AVG",2),rep("HS",2),
          rep("AVG",2),rep("HS",6),rep("AVG",1),rep("HS",5),"SNAIVE",rep("HS",2),"SNAIVE",rep("HS",3),rep("SNAIVE",3),rep("AVG",2),
          rep("HS",3),rep("AVG",1),rep("HS",3),rep("AVG",1),rep("HS",10),rep("AVG",1),"SNAIVE",
          rep("HS",2),"SNAIVE","HS","AVG",rep("HS",2),rep("AVG",2),rep("HS",5),"SNAIVE","AVG",
          "HS","NAIVE","HS","SNAIVE",rep("HS",8),"NAIVE",rep("HS",6),rep("HS",15),"SNAIVE",
          "AVG",rep("NAIVE",2),"HS","SNAIVE",rep("HS",3),"AVG",rep("HS",2),"ETS",rep("HS",8))

pcsWk20AD$Level<-Method

FBVZWW1MAD<-pcsWk20AD %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Level,label=Districts)) +
  geom_point(position=position_jitter(h=0.1, w=0.1),size=2) +
  theme(aspect.ratio = 1)+
  labs(x="PC1", y = "PC2")
FBVZWW1MAD<-FBVZWW1MAD+ theme(legend.position = "right",legend.box = "vertical")+
  labs(colour = NULL)+scale_color_manual(values = c("#e7298a","#e6ab02","#1b9e77","#762a83","#543005"))
FBVZWW1MAD

```


---