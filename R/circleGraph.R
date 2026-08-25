#circleGraph <- function(edges=edgesMid, vertices=vertices, inSeed=3){
inSeed <- 3


vertices1 <- vertices %>%
  mutate(newValue=case_when(
    name=="GEOG" ~ 3,
    TRUE ~ newValue
  ))

vertices1 <- vertices1 %>%
  mutate(newValue=case_when(
    (name=="HC") & (shortName=="top") ~ 3,
    (name=="DG") & (shortName=="top") ~ 3,
    (name=="MB") & (shortName=="top") ~ 3,
    (name=="CB") & (shortName=="top") ~ 3,
    (name=="AK") & (shortName=="top") ~ 3,
    (name=="BG") & (shortName=="top") ~ 3,
    TRUE ~ newValue
  ))

vertices1 <- vertices1 %>%
  mutate(newValue=case_when(
    (from=="AK;CB") & (level==4) ~ 3,
    (from=="AK;CT") & (level==4) ~ 3,
    (from=="AK;HC") & (level==4) ~ 3,
    (from=="BG;TF") & (level==4) ~ 3,
    (from=="CB;DC") & (level==4) ~ 3,
    (from=="CB;TF") & (level==4) ~ 3,
    (from=="BR;OF") & (level==4) ~ 3,
    (from=="CT;DG") & (level==4) ~ 3,
    TRUE ~ newValue
  ))

vertices1 <- vertices1 %>%
  mutate(newValue=case_when(
    (shortName=="BG;IG;TF") & (level==4) ~ 3,
    TRUE ~ newValue
  ))
vertices1 <- vertices1 %>%
  mutate(newValue=case_when(
    #(from=="IG;TF") & (level==4) ~ 5,
    (from=="AK;ZC") & (level==4) ~ 5,
    (from=="CT;ZZ") & (level==4) ~ 7,
    (from=="CT;LS") & (level==4) ~ 7,
    (from=="GA;ZZ") & (level==4) ~ 7,
    (from=="CB;WJ") & (level==4) ~ 7,
    (from=="CB;ME") & (level==4) ~ 7,
    (from=="CB;CH") & (level==4) ~ 7,
    (from=="CL;RD") & (level==4) ~ 7,
    (from=="JC;WJ") & (level==4) ~ 7,
    (from=="GA;IG") & (level==4) ~ 7,
    (from=="LZ;ZZ") & (level==4) ~ 7,
    (from=="MB;ZC") & (level==4) ~ 7,
    (from=="MB;TF") & (level==4) ~ 7,
    (from=="DG;ZZ") & (level==4) ~ 7,
    (from=="DG;LZ") & (level==4) ~ 7,
    (from=="OF;RB") & (level==4) ~ 7,
    (from=="ZC;ZZ") & (level==4) ~ 7,
    (from=="BG;ZZ") & (level==4) ~ 7,
    (from=="BG;JS") & (level==4) ~ 7,
    (from=="JL;JW") & (level==4) ~ 7,
    (from=="LS;LZ") & (level==4) ~ 7,
    (from=="LZ;SL") & (level==4) ~ 7,
    (from=="CB;ZZ") & (level==4) ~ 7,
    (from=="JS;TF") & (level==4) ~ 7,
    (from=="BG;TF") & (level==4) ~ 7,
    (from=="IG;TF") & (level==4) ~ 7,
    (from=="AK;ZC") & (level==4) ~ 7,
    (from=="DG;EK") & (level==4) ~ 7,
    (from=="OF;SQ") & (level==4) ~ 7,
    (from=="CB;TF") & (level==4) ~ 7,
    #(from=="AK;ZC") & (level==5) ~ 7,
    TRUE ~ newValue
  ))

vertices1 <- vertices1 %>%
  mutate(newValue=case_when(
    (from=="CL;OF;SQ") & (level==5) ~ 7,
    (from=="BB;JC;WJ") & (level==5) ~ 7,
    (from=="BR;OF;SQ") & (level==5) ~ 7,
    (from=="AK;HC;LZ") & (level==5) ~ 7,
    (from=="AK;CT;DG") & (level==5) ~ 7,
    (from=="CT;DG;TR") & (level==5) ~ 7,
    (from=="AK;CB;WJ") & (level==5) ~ 7,
    (from=="CB;TF;WJ") & (level==5) ~ 7,
    (from=="BG;TF;ZZ") & (level==5) ~ 7,
    (from=="BG;DG;ZZ") & (level==5) ~ 7,
    (from=="BG;IG;TF") & (level==5) ~ 7,
    (from=="CB;DC;TF") & (level==5) ~ 7,

    TRUE ~ newValue
  ))

# vertices1 <- vertices1 %>%
#   mutate(newValue==case_when(
#     (name=="BG;TF;ZZ") & (level==4) ~ 3,
#     TRUE ~ newValue
#   ))

  mygraph <- graph_from_data_frame(edgesMid, vertices=vertices1 )

  #set seed here to ensure that the graphi is fixed between realizations.
  set.seed(inSeed)
  outGraph <- ggraph(mygraph, layout = 'circlepack', weight=size) +
    #geom_node_circle(aes(fill = as.factor(level), color = as.factor(level) )) +
    geom_node_circle(aes(fill = as.factor(newValue), color = as.factor(newValue) )) +
    #geom_node_circle(aes(fill = as.factor(theColor), color = as.factor(theColor) )) +
    geom_node_label( aes(label=shortName, filter=leaf)) +

  scale_fill_manual(values=c(
    "0"="tan",
    "1"="blue",
    "2"="green",
    "3"="white",
    "4"="pink",
    "5"="yellow",
    "6"="purple",
    "7"="orange"
  )

  ) +
    scale_color_manual( values=c("top" = "green", "1" = "blue", "2" = "yellow", "3" = "green", "4"="white") ) +
    theme_void() +
    theme(legend.position="FALSE")
    #theme(legend.position="bottom")


  outGraph
#}
