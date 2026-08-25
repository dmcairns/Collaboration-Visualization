outData <- t.pc %>%
  #filter(newCol != "JS;TF") %>%
  separate_wider_delim(cols=newCol, delim=";",
                       names_sep="_", too_few="align_start") %>%
  mutate(Lvl1=newCol_1) %>%
  mutate(Lvl2=paste0(Lvl1,";",newCol_2)) %>%
  mutate(Lvl3=case_when(!is.na(newCol_3) == TRUE ~ paste0(Lvl2,";",newCol_3),
                        TRUE ~ Lvl2)) %>%
  mutate(Lvl3=case_when(Lvl3==Lvl2 ~ "",
                        TRUE ~ Lvl3)) %>%
  mutate(Lvl4=case_when(((!is.na(newCol_4) == TRUE) & (Lvl3 != "")) ~ paste0(Lvl3, ";", newCol_4),
                        TRUE ~ Lvl3)) %>%
  mutate(Lvl4=case_when(Lvl4==Lvl3 ~ "",
                        TRUE ~ Lvl4))

Level1 <- outData %>%
  ungroup() %>%
  select("from"="Lvl1", "to"="Lvl2") %>%
  unique()
Level2 <- outData %>%
  ungroup() %>%
  select("from"="Lvl2", "to"="Lvl3") %>%
  unique() %>%
  filter(to != "")
Level3 <- outData %>%
  ungroup() %>%
  select("from"="Lvl3", "to"="Lvl4") %>%
  unique() %>%
  filter(to != "")

Level4 <- t.pc %>%
  #filter(newCol != "JS;TF") %>%
  ungroup() %>%
  mutate(Lvl5=paste0("PUB", row_number())) %>%
  select("from"=newCol, "to"=Lvl5)

edges <- rbind(Level1, Level2, Level3, Level4)
vertices <- t.pc %>%
  ungroup() %>%
  mutate(name=paste0("PUB", row_number())) %>%
  mutate(size=rep(1, nrow(.))) %>%
  select("name", "size")
additionalEdges <- data.frame(name=setdiff(edges$to, vertices$name), size=1)
vertices <- rbind(vertices, additionalEdges)
additionalEdges <- data.frame(name=setdiff(edges$from, vertices$name), size=1)
vertices <- rbind(vertices, additionalEdges)

edgeNames <- edges %>%
  mutate(shortName=case_when(substr(to,1,3)=="PUB" ~ from,
                             TRUE ~ to))
vertices <- vertices %>%
  left_join(edgeNames,by=c("name"="to")) %>%
  mutate(shortName=case_when(is.na(shortName) ~ "top",
                             TRUE ~ shortName))


# edges1 <-
#   rbind(data.frame(from=c(rep("GEOG",18),
#                           rep("IG",1),
#                           rep("IG-TF",2),
#                           rep("AK", 4),
#                           rep("AK-HC", 1),
#                           rep("AK-CB", 1),
#                           rep("AK-CT", 1),
#                           rep("AK-ZC", 3)),
#
#
#                    to=c("IG", "AK", "JC", "CB", "LZ", "BR", "GA","CL","ZC", "BB",
#                         "OF", "BG", "MB", "JL", "DG", "LS", "HC", "CT","IG-TF","PUB1", "PUB2",
#                         "AK-HC", "AK-CB","AK-CT","AK-ZC", "AK-HC-LZ", "AK-CB-WJ", "AK-CT-DG", "PUB3", "PUB4","PUB5")))
#
#
includeFaculty <- c("BR", "IG", "AK", "JC", "MB", "JL", "DG", "LS", "HC",
                    "CL", "CT", "OF", "GA", "BB", "ZC", "CB", "LZ", "BG")
edgesMid <- edges %>%
  mutate(topFrom=substr(from,1,2)) %>%
  filter(topFrom %in% includeFaculty) %>%
  select("from", "to")

t.data <- data.frame(from=rep("GEOG", length(includeFaculty)), to=includeFaculty)
edgesMid <- rbind(edgesMid, t.data)


tree <- FromDataFrameNetwork(edgesMid)

# Then I can easily get the level of each node, and add it to the initial data frame:
mylevels <- data.frame( name=tree$Get('name'), level=tree$Get("level") )
vertices <- vertices %>%
  left_join(., mylevels, by=c("name"="name"))
#t.1 <- data.frame(name="GEOG", size=1, from=1, shortName=1, theColor=1, level=1)
t.1 <- data.frame(name="GEOG", size=1, from=1, shortName=1, level=1)
vertices <- rbind(vertices, t.1)

vertices <- vertices %>%
  mutate(newValue=level)
