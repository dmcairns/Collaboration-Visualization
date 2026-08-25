##############################################
# Mapping collaborations among GEOG faculty  #
##############################################
library("ggvenn")
library(VennDiagram)
library(PlaneGeometry)
library(gsl)

#source(".//Google Drive//My Drive//ShinyApps//AnnualEvaluations//R//readInputFunctions.R")
source(".//R//readInputFunctions.R")
facultyRank <- queryDataFromRemoteDB("facultyRank", c(2000:2022))
facultyNames <- facultyRank %>%
  select("Faculty", "shortName", "shortNameNoSpace", "UIN") %>%
  unique() %>%
  mutate(shortName=case_when(shortName=="O?Reilly" ~ "O'Reilly",
                             shortName=="O?Brien" ~ "O'Brien",
                             shortName=="B. Guneralp" ~ "Guneralp",
                             shortName=="I. Guneralp" ~ "Guneralp",
                             shortName=="S. Bednarz" ~ "Bednarz",
                             shortName=="R. Bednarz" ~ "Bednarz",
                             shortName=="Z. Zhang" ~ "Zhang",
                             shortName=="P. Zhang" ~ "Zhang",
                             TRUE ~ shortName)) %>%
  mutate(firstName=sub(".*, ", "", Faculty)) %>%
  mutate(firstInitial=substr(firstName, 1,1)) %>%
  mutate(lastInitial=substr(Faculty,1,1)) %>%
  mutate(initials=paste0(firstInitial,lastInitial)) %>%
  select("Faculty", "shortName", "shortNameNoSpace", "UIN", "initials")

collaborationPath <- ".//Google Drive//My Drive//ShinyApps//LocalDataArchive//GEOG_Collaboration"
collaborationPath <- "..//LocalDataArchive//GEOG_Collaboration"
journalFile <- "publications-conference-proceedings-patents-and-creative-products-innovations-journal-article_Export.csv"
chapterFile <- "publications-conference-proceedings-patents-and-creative-products-innovations-chapter_Export.csv"
proceedingsFile <- "publications-conference-proceedings-patents-and-creative-products-innovations-conference-proceedings_Export.csv"

journalData <- readInterfolioBulkDownloadFile(collaborationPath,
                                              calYear=NULL,
                                              journalFile)

chapterData <- readInterfolioBulkDownloadFile(collaborationPath,
                                              calYear=NULL,
                                              chapterFile)

proceedingsData <- readInterfolioBulkDownloadFile(collaborationPath,
                                                  calYear=NULL,
                                                  proceedingsFile)
#####################################
# Functions                         #
#####################################

matchAuthors <- function(targetString, authorList){
  # fix problem with multiple Guneralps
  # fix problem with Cairns/Cai
  #
  coAuthors <- NULL
  for(i in 1:nrow(authorList)){

    checkResult <- grep(authorList[i,"shortName"], targetString)
    if(length(checkResult)>0){
      if(!is.null(coAuthors)){
        coAuthors <- paste0(coAuthors, ";", authorList[i, "UIN"])
      } else {
        coAuthors <- authorList[i, "UIN"]
      }
    }
  }
  if(is.null(coAuthors)){
    coAuthors <- NA
  }
  coAuthors
}

arrangeAuthorInitials <- function(inData){
  ###########################################
  # Arrange author initials alphabetically  #
  ###########################################

  outData <- inData %>%
    separate_wider_delim(cols=authorUINs, delim=";",
                         names_sep="_", too_few="align_start") %>%
    pivot_longer(cols=starts_with("authorUIN"), names_to="authorTemp") %>%
    filter(!is.na(value)) %>%
    arrange(UID, value) %>%
    group_by(UID) %>%
    mutate(id = row_number()) %>%
    mutate(authorTemp=paste0("authorUIN_",id)) %>%
    select(-"id") %>%
    pivot_wider(names_from=authorTemp, values_from=value) %>%
    unite(col="newCol", starts_with("authorUIN"), sep=";", na.rm=TRUE)

  outData
}

possibleCollaborators <- function(inData, facultyList){
  ##########################################
  # Determine if more than one author      #
  ##########################################
  processedData <- inData %>%
    mutate(numAuthors=str_count(Authors, ";")+1) %>%
    filter(numAuthors>1) %>%
    select("UID", "Authors", "numAuthors") %>%
    rowwise() %>%
    mutate(authorUINs=matchAuthors(Authors, facultyList)) %>%
    mutate(numCoAuthors=str_count(authorUINs, ";")) %>%
    filter(numCoAuthors>0) %>%
    select("UID", "Authors", "authorUINs") %>%
    mutate(authorUINs=case_when(((str_count(authorUINs, "702002859")==1) &
                                (str_count(authorUINs, "622009903")==1)) ~ str_remove(authorUINs, "622009903"),
                               TRUE ~ authorUINs)) %>%
    mutate(authorUINs=sub(";$", "", authorUINs)) %>%
    mutate(authorUINs=case_when(((str_count(authorUINs, "702002859")==1) &
                                   (str_count(authorUINs, "901006717")==1)) ~ str_remove(authorUINs, "901006717"),
                                TRUE ~ authorUINs)) %>%
    mutate(authorUINs=sub(";$", "", authorUINs)) %>%
    mutate(numAuthors=str_count(authorUINs, ";")+1) %>%
    filter(numAuthors>1) %>%
    select("UID", "Authors", "authorUINs") %>%
    mutate(inciCount=case_when(((str_count(authorUINs, "918003521")>0) &
                                (str_count(Authors, "Inci") > 0)) ~ 1,
                               TRUE ~ 0)) %>%
    mutate(authorUINs=case_when(inciCount==0 ~ str_remove(authorUINs, "918003521"),
                                TRUE ~ authorUINs)) %>%
    mutate(authorUINs=sub(";$", "", authorUINs)) %>%
    select("UID", "Authors", "authorUINs") %>%
    mutate(burakCount=case_when(((str_count(authorUINs, "320000465")>0) &
                                  (str_count(Authors, "Burak") > 0)) ~ 1,
                               TRUE ~ 0)) %>%
    mutate(authorUINs=case_when(burakCount==0 ~ str_remove(authorUINs, "320000465"),
                                TRUE ~ authorUINs)) %>%
    mutate(authorUINs=sub(";$", "", authorUINs)) %>%
    select("UID", "Authors", "authorUINs") %>%
    mutate(numAuthors=str_count(authorUINs, ";")+1) %>%
    filter(numAuthors>1) %>%
    select("UID", "Authors", "authorUINs") %>%
    mutate(authorUINs=sub(";;", ";", authorUINs)) %>%
    mutate(authorUINs=gsub("^;","",authorUINs)) %>%
    mutate(numAuthors=str_count(authorUINs, ";")+1) %>%
    filter(numAuthors>1)

  for(i in 1:nrow(facultyList)){
    processedData <- processedData %>%
      mutate(authorUINs=str_replace(authorUINs, facultyList$UIN[i], facultyList$initials[i])) %>%
      mutate(authorUINs=str_replace(authorUINs, "PZ", "")) %>%
      mutate(authorUINs=str_replace(authorUINs, "SC", "")) %>%
      mutate(authorUINs=sub(";$", "", authorUINs)) %>%
      mutate(authorUINs=sub(";;", ";", authorUINs)) %>%
      mutate(authorUINs=gsub("^;","",authorUINs)) %>%
      mutate(numAuthors=str_count(authorUINs, ";")+1) %>%
      filter(numAuthors>1) %>%
      select("UID", "Authors", "authorUINs")
  }
  processedData <- arrangeAuthorInitials(processedData) %>%
    filter(newCol != "OF;ZZ")

  processedData
}


t.pc <- possibleCollaborators(journalData, facultyNames)


# I don't think that the vennDiagram is what I need.
createCollaborationMatrix <- function(inData){
  outData <- inData %>%
    rename("authorUINs"="newCol") %>%
    separate_wider_delim(cols=authorUINs, delim=";",
                         names_sep="_", too_few="align_start") %>%
    pivot_longer(cols=starts_with("authorUIN"), names_to="authorTemp") %>%
    filter(!is.na(value)) %>%
    arrange(UID, value) %>%
    group_by(UID) %>%
    mutate(id = row_number()) %>%
    mutate(authorTemp=paste0("authorUIN_",id)) %>%
    select(-"id") %>%
    pivot_wider(names_from=authorTemp, values_from=value)

  ##################################################
  # The first two columns of outData are UID and   #
  # Authors.  The remaining columns are author     #
  # abbreviations.                                 #
  ##################################################

  abbrvMatrix <- outData[,c(3:ncol(outData))]

  allAuthors <- sort(unique(as.vector(as.matrix(abbrvMatrix))))

  numCol <- length(allAuthors)
  numRow <- length(allAuthors)
  theMatrix <- matrix(data=0, nrow=numRow, ncol=numCol)
  row.names(theMatrix) <- allAuthors
  colnames(theMatrix) <- allAuthors

  for(i in 1:nrow(abbrvMatrix)){
    for(j in 1:ncol(abbrvMatrix)){
      theAuthor <- as.vector(unlist(abbrvMatrix[i,j]))
      if(!is.na(theAuthor)){
        theTargets <- row.names(theMatrix) %in% abbrvMatrix[i,]
        theMatrix[theAuthor,theTargets] <- theMatrix[theAuthor,theTargets]+1
      }
    }
  }

  theMatrix

}
vennData <- list('DC'=c("CL-DC", "CL-DC", "AF-DC"),
                 'CL'=c("CL-DC", "CL-SQ")
                 )
ggvenn(vennData, show_percentage=TRUE,
       stroke_color="red",
       stroke_linetype="solid",
       auto_scale=TRUE)

grid.newpage()
draw.pairwise.venn(area1=20, area2=45,cross.area=10,
                   category=c("Mango","Banana"),fill=c("Red","Yellow"))


sharedArea <- function(A, B) {

  d <- gsl::hypot(B$x - A$x, B$y - A$y)
  #if (d <= abs(B$r - A$r)) return (pi * min(a, b))
  if (d==0) {
    theOutput <-  pi*A$r^2
  } else {
    if ((B$x-B$r) <= (A$x-A$r)) {
      theOutput <- pi*A$r^2
    } else {
    if ((d>0) & (d < A$r + B$r)) {
      a <- A$r^2
      b <- B$r^2
      x <- (a - b + d^2) / (2 * d)
      z <- x^2
      y <- sqrt(a - z)
      theOutput <-  (a * asin(y / A$r) + b * asin(y / B$r) - y * (x + sqrt(z + b - a)))
    } else {
      theOutput <- 0
    }}}

  theOutput
}

circle_intersection <- function(A,B){
  x1 <- A$x
  y1 <- A$y
  r1 <- A$r
  x2 <- B$x
  y2 <- B$y
  r2 <- B$r
  rr1 <- r1 * r1
  rr2 <- r2 * r2
  d <- sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

  if (d > r2 + r1) # Circles do not overlap
  {
    return(0)
  } else if (d <= abs(r1 - r2) && r1 >= r2){ # Circle2 is completely inside circle1
    return(pi*rr2)
  } else if (d <= abs(r1 - r2) && r1 < r2){ # Circle1 is completely inside circle2
    return(pi*rr1)
  } else { # Circles partially overlap
    phi <- (acos((rr1 + (d * d) - rr2) / (2 * r1 * d))) * 2
    theta <- (acos((rr2 + (d * d) - rr1) / (2 * r2 * d))) * 2
    area2 <- 0.5 * theta * rr2 - 0.5 * rr2 * sin(theta)
    area1 <- 0.5 * phi * rr1 - 0.5 * rr1 * sin(phi)
    return(area1 + area2)
  }
}
A <- data.frame(x = 3, y = 4, r = 2)
B <- data.frame(x = 6, y = 5, r = 1.75)

findSeparation <- function(radiusCircleA, radiusCircleB, targetPercentage){
  ###########################################
  #                                         #
  # Numerically estimate what distance will #
  # result in a specified proportion of     #
  # overlap of Circle A between Circle A    #
  # and Circle B.                           #
  ###########################################

  ##############################################
  # Percent overlap will be 100% when distance #
  # between centers is zero.  Percent overlap  #
  # will be 0% when distance between centers   #
  # equals the radius of circle A.             #
  ##############################################

  areaCircleA <- pi*radiusCircleA^2
  areaCircleB <- pi*radiusCircleB^2

  # set a range of distances between the two centers
  # (0 to radiusCircleA+radiusCircleB)
  # assume that circleA has a center at (0,0)
  #  https://community.rstudio.com/t/about-circle-overlap-area/165896/4

  centerDistances <- c(0:100)/100*(radiusCircleA+radiusCircleB)
  circleA <- data.frame(x=0, y=0, r=radiusCircleA)

  areasByDistance <- c(NULL)
  for(i in 1:length(centerDistances)){
    circleB <- data.frame(x=centerDistances[i], y=0, r=radiusCircleB)
    percentOfCircleA <- circle_intersection(circleA, circleB)/areaCircleA
    areasByDistance <- c(areasByDistance, percentOfCircleA)
  }
  theOutput <- data.frame(distance=centerDistances, areaPercentage=areasByDistance)
  theOutput <- theOutput %>%
    mutate(diff=areaPercentage-targetPercentage) %>%
    filter(abs(diff)==min(abs(diff), na.rm=TRUE)) %>%
    pull(distance) %>%
    mean()

  theOutput

}

t.1 <- findSeparation(1,1.5, 0.5)




drawTwoCircles <- function(circ1, circ2){
  O1 <- circ1$center
  O2 <- circ2$center
  intersections <- intersectionCircleCircle(circ1, circ2)
  A <- intersections[[1]]; B <- intersections[[2]]
  theta1 <- Arg((A-O1)[1] + 1i*(A-O1)[2])
  theta2 <- Arg((B-O1)[1] + 1i*(B-O1)[2])
  path1 <- Arc$new(O1, circ1$radius, theta1, theta2, FALSE)$path()
  theta1 <- Arg((A-O2)[1] + 1i*(A-O2)[2])
  theta2 <- Arg((B-O2)[1] + 1i*(B-O2)[2])
  path2 <- Arc$new(O2, circ2$radius, theta2, theta1, FALSE)$path()
  plot(0, 0, type="n", xlim = c(0,100), ylim = c(0,100),xlab = NA, ylab = NA)
  grid()
  draw(circ1, border = "blue", lwd = 2)
  draw(circ2, border = "forestgreen", lwd = 2)
  polypath(rbind(path1,path2), col = "red")
  points(3,4,pch=19, col = "blue")
  # text(3,5,"A")
  # text(3,3.55,"r=2")
  # points(6,5,pch=19, col = "forestgreen")
  # text(6,6,"B")
  # text(6,4.5,"r=1.75")
}



drawOffsetCircles <- function(offset, r1, r2){
  xOrigin <- 50
  yOrigin <- 50
  A <- data.frame(x = xOrigin, y = yOrigin, r = r1)
  B <- data.frame(x = xOrigin + offset, y = yOrigin, r = r2)
  O1 <- c(A$x,A$y); circ1 <- Circle$new(O1, A$r)
  O2 <- c(B$x,B$y); circ2 <- Circle$new(O2, B$r)
  drawTwoCircles(circ1, circ2)
}

grid.newpage()
drawOffsetCircles(findSeparation(1,1.5, .9), 1, 1.5)

findSharedArea <- function(offset, r1, r2, percent=TRUE){
  #################################################
  # find the percentage of the first circle (r1)  #
  # that is overlapped by second circle (r2)      #
  #                                               #
  #################################################
  A <- data.frame(x=50, y=50, r=r1)
  B <- data.frame(x=50+offset, y=50, r=r2)
  if(percent==TRUE){
    theOutput <- circle_intersection(A,B)/(pi*A$r^2)
  } else {
    theOutput <- circle_intersection(A,B)
  }
  theOutput
}

drawAuthorCollaboration <- function(inMatrix, authorInitials){
  ###############################################
  # Draw collaborations among faculty authors   #
  ###############################################
  inData <- data.frame(inMatrix)
  colSym <- rlang::sym(authorInitials)
  theAuthorData <- inData %>%
    select(all_of(authorInitials)) %>%
    filter(!!colSym >0)  %>%
    mutate(authorInitials=row.names(.))
  collabPubs <- inData %>%
    pivot_longer(cols=everything(), names_to="authors") %>%
    group_by(authors) %>%
    summarise(numPubs=max(value))
  theAuthorData <- theAuthorData %>%
    left_join(collabPubs, by=c("authorInitials"="authors")) %>%
    rename("circleSize"="numPubs") %>%
    mutate(pctOverlap=!!colSym/max(!!colSym))


  theAuthorData
}
drawAuthorCollaboration2 <- function(pubListLong, inSeed=1){
  ###############################################
  # Draw collaborations among faculty authors   #
  ###############################################
  outData <- pubListLong %>%
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

  vertices <- vertices %>%
    mutate(theColor=shortName) %>%
    mutate(theColor=case_when((shortName=="CL;DC;JJ;KG" & (from == "CL;DC;JJ;KG")) ~ "top1",
                              ((from=="CL") & (shortName=="CL;DC")) ~ "top4",
                              ((from=="OF") & (shortName=="OF;RB")) ~ "top4",
                              ((from=="HC") & (shortName=="HC;LZ")) ~ "top4",
                              ((from=="IG") & (shortName=="IG;TF")) ~ "top4",
                              ((from=="LS") & (shortName=="LS;LZ")) ~ "top5",
                              ((from=="CB") & (shortName=="CB;WJ")) ~ "top4",
                              ((from=="CB") & (shortName=="CB;TF")) ~ "top4",
                              ((from=="CB") & (shortName=="CB:DC")) ~ "top4",
                              ((from=="GA") & (shortName=="GA;ZZ")) ~ "top4",
                              ((from=="DG") & (shortName=="DG;ZZ")) ~ "top4",
                              ((from=="MB") & (shortName=="MB;ZC")) ~ "top4",
                              ((from=="MB") & (shortName=="MB;TF")) ~ "top4",
                              ((from=="CT") & (shortName=="CT;ZZ")) ~ "top4",
                              ((from=="CT") & (shortName=="CT;LS")) ~ "top4",
                              ((from=="CT") & (shortName=="CT;DG")) ~ "top6",
                              ((from=="BG") & (shortName=="BG;TF")) ~ "top4",
                              ((from=="BG") & (shortName=="BG;IG")) ~ "top4",
                              ((from=="LZ") & (shortName=="LZ;ZZ")) ~ "top4",
                              ((from=="BG") & (shortName=="BG;ZZ")) ~ "top4",
                              ((from=="JC") & (shortName=="JC;WJ")) ~ "top4",
                              ((from=="ZC") & (shortName=="ZC;ZZ")) ~ "top4",
                              ((from=="CL;DC") & (shortName=="CL;DC")) ~ "top6",
                              ((from=="CL;DC") & (shortName=="CL;DC;JJ")) ~ "top7",
                              ((from=="CL;OF") & (shortName=="CL;OF;SQ")) ~ "top4",
                              ((from=="CT;DG") & (shortName=="CT;DG;TR")) ~ "top4",
                              ((from=="BR;OF") & (shortName=="BR;OF;SQ")) ~ "top4",
                              ((from=="AK") & (shortName=="AK;ZC")) ~ "top4",
                              ((from=="AK") & (shortName=="AK;CT")) ~ "top4",
                              ((from=="AK") & (shortName=="AK;CB")) ~ "top4",
                              ((from=="AK") & (shortName=="AK;HC")) ~ "top4",
                              ((from=="AK;HC") & (shortName=="AK;HC;LZ")) ~ "top6",
                              ((from=="AK;CT") & (shortName=="AK;CT;DG")) ~ "top6",
                              ((from=="AK;CB") & (shortName=="AK;CB;WJ")) ~ "top5",
                              ((from=="BG") & (shortName=="BG;DG")) ~ "top4",
                              ((from=="BB") & (shortName=="BB;JC")) ~ "top4",
                              ((from=="CB") & (shortName=="CB;DC")) ~ "top4",
                              shortName=="JS;TF" ~ "top5",
                              shortName=="OF;RB" ~ "top5",
                              shortName=="HC;LZ" ~ "top5",
                              shortName=="OF;SQ" ~ "top5",
                              shortName=="HC;ZC" ~ "top5",
                              shortName=="CL;RD" ~ "top5",
                              shortName=="JL;JW" ~ "top5",
                              shortName=="BB;JC;WJ" ~ "top5",
                              shortName=="IG;TF" ~ "top5",
                              shortName=="GA;ZZ" ~ "top5",
                              shortName=="CB;CH" ~ "top5",
                              shortName=="CB;ME" ~ "top5",
                              shortName=="CB;ZZ" ~ "top5",
                              shortName=="CB;TF;WJ" ~ "top5",
                              shortName=="CB;DC;TF" ~ "top5",
                              shortName=="MB;ZC" ~ "top5",
                              shortName=="MB;TF" ~ "top5",
                              shortName=="DG;ZZ" ~ "top5",
                              shortName=="DG;EK" ~ "top5",
                              shortName=="DG;LZ" ~ "top5",
                              shortName=="CT;ZZ" ~ "top5",
                              shortName=="CT;LS" ~ "top5",
                              shortName=="CB;WJ" ~ "top5",
                              shortName=="BG;TF" ~ "top5",
                              shortName=="GA;IG" ~ "top5",
                              shortName=="BG;IG" ~ "top5",
                              shortName=="BG;ZZ" ~ "top5",
                              shortName=="LZ;ZZ" ~ "top5",
                              shortName=="LZ;SL" ~ "top5",
                              shortName=="CL;DC" ~ "top5",
                              shortName=="BG;JS" ~ "top5",
                              shortName=="JC;WJ" ~ "top5",
                              shortName=="ZC;ZZ" ~ "top5",
                              shortName=="CL;OF;SQ" ~ "top5",
                              shortName=="CT;DG;TR" ~ "top5",
                              shortName=="CL;DC;JJ;KG" ~ "top5",
                              shortName=="BR;OF;SQ" ~ "top5",
                              shortName=="AK;HC;LZ" ~ "top5",
                              shortName=="AK;CB;WJ" ~ "top5",
                              shortName=="AK;CT;DG" ~ "top5",
                              shortName=="AK;ZC" ~ "top5",
                              shortName=="BR;GA;OF;SQ" ~ "top5",
                              shortName=="LS;LZ" ~ "top5",
                              TRUE ~ shortName))

  browser()
  mygraph <- graph_from_data_frame( edges, vertices=vertices )

  #set seed here to ensure that the graphi is fixed between realizations.
  set.seed(inSeed)
  ggraph(mygraph, layout = 'circlepack', weight=size) +
    #geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
    geom_node_circle(aes(fill = as.factor(theColor), color = as.factor(theColor) )) +
    #geom_node_circle() +
    geom_node_label( aes(label=shortName, filter=leaf)) +
    #scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
    scale_fill_manual(values=c("top" = "tan", "top1"="blue", "top2"="green", "top3"="white", "top4"="pink", "top5"="yellow", "top6"="purple", "top7"="green")) +
    scale_color_manual( values=c("top" = "green", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
    theme_void() +
    theme(legend.position="FALSE")

  #browser()
  #print(setdiff(edges$to, vertices$name))

}

drawAuthorCollaboration2(t.pc, inSeed=3)


drawAuthorCollaboration(t.m, "DC")

drawOffsetCircles(findSeparation(20,20,0.9), 20, 20)
drawOffsetCircles(findSeparation(20,29,0.1), 20, 29)

############################################
# Circular packing graphs                  #
############################################

#https://r-graph-gallery.com/315-hide-first-level-in-circle-packing.html
library(ggraph)
library(igraph)
library(viridis)
#library(tidyverse)
# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
edges <- flare$edges

# Usually we associate another dataset that give information about each node of the dataset:
vertices <- flare$vertices

# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(mygraph, layout = 'circlepack') +
  geom_node_circle() +
  theme_void()

# ggraph(mygraph, layout = 'circlepack', weight=size) +
#   geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
#   scale_fill_manual(values=c("0" = "tan", "1" = "blue", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
#   scale_color_manual( values=c("0" = "tan", "1" = "blue", "2" = "black", "3" = "black", "4"="black") ) +
#   theme_void() +
#   theme(legend.position="FALSE")

ggraph(mygraph, layout = 'circlepack', weight=size ) +
  geom_node_circle(aes(fill = depth)) +
  geom_node_label( aes(label=shortName, filter=leaf, size=size)) +
  theme_void() +
  theme(legend.position="FALSE") +
  scale_fill_viridis()

edges <- data.frame(from=c("DC", "DC", "CB;DC", "CL;DC","CL;DC", "CL;DC", "CL;DC", "CB;DC;TF", "CB;DC;TF"), to=c("CL;DC", "CB;DC", "CB;DC;TF", "CL;DC;JJ;KG", "PUB1", "PUB2", "PUB3", "PUB4", "PUB5"))
vertices <- data.frame(name=c("CL;DC","CB;DC;TF", "CL;DC;JJ;KG", "DC", "CB;DC", "PUB1", "PUB2", "PUB3", "PUB4", "PUB5"), size=(c(1,1,1, 1, 1, 1, 1,1,1,1)))

edgeNames <- edges %>%
  mutate(shortName=case_when(substr(to,1,3)=="PUB" ~ from,
                             TRUE ~ to))
vertices <- vertices %>%
  left_join(edgeNames,by=c("name"="to"))



# Then we have to make a 'graph' object using the igraph library:
mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'circlepack', weight=size ) +
  geom_node_circle(aes(fill = depth)) +
  geom_node_label( aes(label=shortName, filter=leaf, size=size)) +
  theme_void() +
  theme(legend.position="FALSE") +
  scale_fill_viridis()
ggraph(mygraph, layout = 'circlepack', weight=size) +
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "tan", "1" = "blue", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "tan", "1" = "blue", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() +
  theme(legend.position="FALSE")
