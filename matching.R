argumentos <- commandArgs()

cuerpo1 <- read.table(argumentos[6], header = TRUE)
cuerpo2 <- read.table(argumentos[7], header = TRUE)

#cuerpo1 <- read.table("bunnyhead/bunnyhead.vertex2.txt", header = TRUE)
#cuerpo2 <- read.table("xvolt/xvolt.vertex.txt", header = TRUE)
########################################################################
border_indices <- function(cuerpo_var, min_or_max) {
   arreglo <- vector()
   if ("upper" == min_or_max) {
      for (i in 1:length(cuerpo_var)) {
         if (cuerpo_var[i] == max(cuerpo_var)) {
            arreglo <- c(arreglo, i) } } } ##optimizar!!!!!!

   if ("lower" == min_or_max) {
      for (i in 1:length(cuerpo_var)) {
         if (cuerpo_var[i] == min(cuerpo_var)) {
            arreglo <- c(arreglo, i) } } } ##optimizar!!!!!!
   else {}
   return(arreglo) }
########################################################################
border_spectre <- function(cuerpo, cuerpo_var, min_or_max) {
   return(subset(cuerpo[border_indices(cuerpo_var, min_or_max),])) }
########################################################################
clockwise_90 <- function(data_frame) {
   original_col_names <- colnames(data_frame)  
   print(original_col_names) 
   colnames(data_frame) <- c("x", "y")

   tmp <- data_frame$x
   data_frame$x <- data_frame$y
   data_frame$y <- tmp    
   data_frame$x <- (-1) * data_frame$x
   
   colnames(data_frame) <- original_col_names
   print(colnames(data_frame))
   return(data_frame) }
########################################################################
vectors_of_2_distance <- function(X_1, Y_1, X_2, Y_2) {
   return(sqrt(((X_2 - X_1) ** 2) + ((Y_2 - Y_1) ** 2))) }
########################################################################
plane_divergence <- function(border_spectre1_abs, border_spectre1_ord, border_spectre2_abs, border_spectre2_ord) {

   X_1vector <- as.vector(t(border_spectre1_abs))
#  print(c("X_1vector",X_1vector))
#  print(c("length(X_1vector)",length(X_1vector)))

   Y_1vector <- as.vector(t(border_spectre1_ord))
#  print(c("Y_1vector",Y_1vector))
#  print(c("length(Y_1vector)",length(Y_1vector)))

   X_2vector <- as.vector(t(border_spectre2_abs))
#  print(c("X_2vector",X_2vector))
#  print(c("length(X_2vector)",length(X_2vector)))

   Y_2vector <- as.vector(t(border_spectre2_ord))
#  print(c("Y_2vector",Y_2vector))
#  print(c("length(Y_2vector)",length(Y_2vector)))

   arre <- vector()

   outer_index <- 0
   for (i in 1:length(X_1vector)) {
      outer_index <- outer_index + 1
      inner_index <- 0
      for (j in 1:length(X_2vector)) {
         inner_index <- inner_index + 1
         if (outer_index == inner_index) {

            arre <- c(arre, vectors_of_2_distance(X_1vector[i],Y_1vector[i],X_2vector[j],Y_2vector[j]))
#           print(c("arre",arre))
         } else {} } }


#  print(c("final arre",arre))
   if (length(arre) <= 1) {
      return(NULL) }
   else {
      return(mean(arre, na.rm = TRUE)) } }
########################################################################
compare_borders <- function(cuerpo_1, var_1, min_or_max1, cuerpo_2, var_2, min_or_max2) {

   if (var_1 == "x") {
      cuerpo1_var <- cuerpo_1$x
      border_spectre_1 <- border_spectre(cuerpo_1, cuerpo1_var, min_or_max1)
      border_spectre_1$x <- NULL
      border_spectre1_abs <- border_spectre_1$y
      border_spectre1_ord <- border_spectre_1$z }

   else if (var_1 == "y") {
      cuerpo1_var <- cuerpo_1$y
      border_spectre_1 <- border_spectre(cuerpo_1, cuerpo1_var, min_or_max1)
      border_spectre1_abs <- border_spectre_1$x
      border_spectre_1$y <- NULL
      border_spectre1_ord <- border_spectre_1$z }

   else if (var_1 == "z") {
      cuerpo1_var <- cuerpo_1$z
      border_spectre_1 <- border_spectre(cuerpo_1, cuerpo1_var, min_or_max1)
      border_spectre1_abs <- border_spectre_1$x
      border_spectre1_ord <- border_spectre_1$y
      border_spectre_1$z <- NULL }
   else{}

   if (var_2 == "x") {
      cuerpo2_var <- cuerpo_2$x
      border_spectre_2 <- border_spectre(cuerpo_2, cuerpo2_var, min_or_max2)
      border_spectre_2$x <- NULL
      border_spectre2_abs <- border_spectre_2$y 
      border_spectre2_ord <- border_spectre_2$z }

   else if (var_2 == "y") {
      cuerpo2_var <- cuerpo_2$y
      border_spectre_2 <- border_spectre(cuerpo_2, cuerpo2_var, min_or_max2)
      border_spectre2_abs <- border_spectre_2$x
      border_spectre_2$y <- NULL
      border_spectre2_ord <- border_spectre_2$z }  

   else if (var_2 == "z") {
      cuerpo2_var <- cuerpo_2$z
      border_spectre_2 <- border_spectre(cuerpo_2, cuerpo2_var, min_or_max2)
      border_spectre2_abs <- border_spectre_2$x
      border_spectre2_ord <- border_spectre_2$y
      border_spectre_2$z <- NULL }
   else{}

   arreglo <- vector()   
 
 # print(border_spectre_1)


   for (i in 1:4) {
   #  print(c("border_spectre_1[1]",border_spectre_1[1]))
 #    print(c("border_spectre_1[2]",border_spectre_1[2]))
 #    print(c("border_spectre_2[1]",border_spectre_2[1]))
 #    print(c("border_spectre_2[2]",border_spectre_2[2]))
      arreglo <- c(c(arreglo), c(plane_divergence(border_spectre_1[1], border_spectre_1[2], border_spectre_2[1], border_spectre_2[2])))
      border_spectre_2 <- clockwise_90(border_spectre_2)
 #    print(c("todataframe",arreglo))
      
       }
   

print(str(cuerpo1))
print(str(cuerpo2))
 
   data_frame <- data.frame(c(var_1), c(min_or_max1), c(var_2), c(min_or_max2),c(arreglo[1]), c(arreglo[2]), c(arreglo[3]), c(arreglo[4]), stringsAsFactors = FALSE)
   colnames(data_frame) <- c("v1","m1","v2","m2","deg_0","deg_90", "deg_180", "deg_270")
    return(data_frame)
 }

############################################################################################

#tatat <- function() {

print(str(cuerpo1))
print(str(cuerpo2))


   merged <- compare_borders(cuerpo1, "z", "lower", cuerpo2, "z", "lower")
   merged <- merge(merged, compare_borders(cuerpo1, "z", "lower", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "lower", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "lower", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "lower", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "lower", cuerpo2, "x", "upper"), all=TRUE)

   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "z", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "z", "upper", cuerpo2, "x", "upper"), all=TRUE)

   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "z", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "lower", cuerpo2, "x", "upper"), all=TRUE)

   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "z", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "y", "upper", cuerpo2, "x", "upper"), all=TRUE)

   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "z", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "lower", cuerpo2, "x", "upper"), all=TRUE)

   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "z", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "z", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "y", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "y", "upper"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "x", "lower"), all=TRUE)
   merged <- merge(merged, compare_borders(cuerpo1, "x", "upper", cuerpo2, "x", "upper"), all=TRUE)

   print(merged)

   min_deg_0 <- subset(merged, deg_0 == min(merged$deg_0[!is.na(merged$deg_0)],na.ra=TRUE), select = c(v1,m1,v2,m2,deg_0))
   min_deg_90 <- subset(merged, deg_90 == min(merged$deg_90[!is.na(merged$deg_90)],na.ra=TRUE), select = c(v1,m1,v2,m2,deg_90))
   min_deg_180 <- subset(merged, deg_180 == min(merged$deg_180[!is.na(merged$deg_180)],na.ra=TRUE), select = c(v1,m1,v2,m2,deg_180))
   min_deg_270 <- subset(merged, deg_270 == min(merged$deg_270[!is.na(merged$deg_270)],na.ra=TRUE), select = c(v1,m1,v2,m2,deg_270))


   print(str(cuerpo1))
   print(str(cuerpo2))

   print(min_deg_0)
   print(min_deg_90)
   print(min_deg_180)
   print(min_deg_270)


   if (as.numeric(min_deg_0$deg_0[1]) < as.numeric(min_deg_90$deg_90[1])) {
      min_0_90 <- min_deg_0
      first_min <- min_0_90$deg_0
   } else {
      min_0_90 <- min_deg_90
      first_min <- min_0_90$deg_90
   }

   print(first_min)

 if ((as.numeric(min_deg_180$deg_180[1]) == NULL || as.numeric(min_deg_270$deg_270[1] == NULL)))

    if (as.numeric(min_deg_180$deg_180[1]) < as.numeric(min_deg_270$deg_270[1])) {
       min_180_270 <- min_deg_180
       second_min <- min_180_270$deg_180
    }else {
       min_180_270 <- min_deg_270
       second_min <- min_180_270$deg_270
    }
 
    print(second_min)

 }
 
 
    if (as.numeric(first_min[1]) < as.numeric(second_min[1])) {
       final_min <- min_0_90
    }else {
       final_min <- min_180_270
    }

  }

final_min <- first_min

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

print(final_min)

 #   v1    m1   v2    m2    deg_0
# 7  x   upper  x    lower     0

col_names <- colnames(final_min)

eje <- final_min$v2
sentido <- final_min$m2
grados <- col_names[5]

print(c("eje",eje))
print(c("sentido",sentido))
print(c("grados",grados))

print(str(cuerpo1))
print(str(cuerpo2))

corpus <- cuerpo2
invert <- function(corpus, final_min, eje) {

   if((eje == "x") && (final_min$m1 == final_min$m2)) {
           corpus$x <- (-1) * corpus$x
   }

   else if((eje == "y") && (final_min$m1 == final_min$m2)) {
           corpus$y <- (-1) * corpus$y
   }

   else if((eje == "x") && (final_min$m1 == final_min$m2)) {
           corpus$z <- (-1) * corpus$z
   } else {}

   return(corpus)
}

corpus <- invert(corpus, final_min, eje)

if(grados != "deg_0") {
    if (grados == "deg_90") {
       vect <- c(1) }

    else  if (grados == "deg_180"){
       vect <- c(1,2) }

    else if (grados == "deg_270") {
       vect <- c(1,2,3)
    } else {}

    if (eje == "x") {
       corpus_tmp <- corpus
       corpus_tmp$x <- NULL
       for (i in 1:length(vect)) {          
            corpus_tmp <- clockwise_90(corpus_tmp) }
       corpus$y <- corpus_tmp$y
       corpus$z <- corpus_tmp$z }

    else if (eje == "y") {
       corpus_tmp <- corpus
       corpus_tmp$y <- NULL
       for (i in 1:length(vect)) {
            corpus_tmp <- clockwise_90(corpus_tmp) }
       corpus$x <- corpus_tmp$x
       corpus$z <- corpus_tmp$z }

    if (eje == "z") {
       corpus_tmp <- corpus
       corpus_tmp$z <- NULL
       for (i in 1:length(vect)) {
            corpus_tmp <- clockwise_90(corpus_tmp) }
       corpus$x <- corpus_tmp$x
       corpus$y <- corpus_tmp$y } }

########################################################

cuerpo_entero <- merge(cuerpo1, corpus, all=TRUE)

fichier <- file(argumentos[8])
write.table(cuerpo_entero, fichier, sep=" ")  
closeAllConnections(fichier)


