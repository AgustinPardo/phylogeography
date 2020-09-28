require(ape);
require(phytools);
require(RColorBrewer);
require(mapdata);

setwd("/home/agustin/workspace/covid-19/mapeo_arbol/arbol_colores_auto/argentina");
phy <- read.tree("ruteado.nwk");
lin <- read.table("linajes", header=FALSE, stringsAsFactors=FALSE);
colnames(lin) <- c("tip", "lineage");
gps<-read.csv("gps_data.dat", header=TRUE);
rownames(gps) <- gps$ID;

latitudes_uniques <- unique(gps$Lat);
#latitudes_uniques <- as.integer(latitudes_uniques);

latitudes_id <- gps[,1:2];
ids <- gps$ID;
#latitudes <- as.integer(gps$Lat);
latitudes <- (gps$Lat);
names(latitudes)=gps$ID;

# colours
colores <- brewer.pal(n=4, name = 'Set1');
colores <- rep("#E41A1C", 115)
names(colores) <- latitudes_uniques;
# display.brewer.pal(7, 'Set1');

n <- length(phy$tip.labels);
colores_vector <- vector(mode="character", length=n);
id_vector <- vector(mode="character", length=n);
for (i in (1:dim(latitudes_id)[1])) {
  colores_vector[i] <- colores[toString(latitudes[i])];
  id_vector[i] <- names(latitudes[i]);
}
names(colores_vector) <- id_vector;

regiones=c('Brazil', 'Colombia', 'Uruguay', 'Chile', 'Argentina', 'Peru', 'Falkland',
           'Paraguay', 'Ecuador', 'Venezuela', 'Guyana', 'Suriname', 'French Guiana')

obj <- phylo.to.map(compute.brlen(phy), gps[1:418,2:3], fsize=0.3, lwd=0.5, pts=FALSE, lty="solid",
                    colors=colores_vector, direction="rightwards", database="worldHires", 
                    regions=c('Argentina','Falkland'),
                    ftype="off", # turn off tip labels
                    psize=0.75, # size of plotted points on the map
                    rotate=FALSE); # hmm why not rotate?

countries <- c('Argentina', 'Brazil', 'Chile', 'Colombia', 'Ecuador', 'Perú', 'Uruguay');
# NS order: Colombia, Ecuador, Peru, Brazil, Uruguay, Chile, Argentina
ns <- c(7, 4, 6, 1, 2, 3, 5); # rank of above vector of countries in N:S order. note this changes with adding/removing regions

# latitudes_uniques: will give order recorded (from gps           )
ord <- match(sort(latitudes_uniques, decreasing=TRUE), latitudes_uniques); # gives N:S ranking in _encountered_ order

# need to get the order of colours correct
cols <- colores[ord[ns]]; # map-to-map (see above)

# map legend
#legend('topright', countries, pt.bg=cols, pch=21, bty="n", cex=0.75, title="Mapa"); # this version has black border around points (like map)

# store parameters from previous tree plotting
lastPP <- get("last_plot.phylo", envir=ape::.PlotPhyloEnv);
yy <- lastPP$yy[1:417]; # y coordinates for all the tips, in the order of phy$tip.label going up
x.min <- min(lastPP$xx); # x coordinate of root (left-most part of the tree)
x.max <- max(lastPP$xx); # x coordinate of tips (all the same)

# make sure lineage info is in same order as tip labels
idx <- match(phy$tip.label, lin$tip);
lin <- lin[idx,];
lin$yy <- yy;
rownames(lin) <- NULL;

length(unique(lin$lineage)); # that's a lot of lineages (and colours required)!
# [1] 20

# in the example figure, only lineages X or X.Y are shown (not X.Y.Z)
# assuming that is what is wanted here, so relabelling
lin$major_lin <- gsub("(\\.\\d)\\..+", "\\1", lin$lineage);

length(unique(lin$lineage)); # much more manageable!
# [1] 9

# colours were not provided, so generating some here
lin.cols <- as.data.frame(cbind(lin=sort(unique(lin$lineage)),
                                col=brewer.pal(n=12, name='Set3')), stringsAsFactors=FALSE);
# if you don't like the colours i selected, replace them in the df above

# figure out where rectangle boundaries are. x0 and x1 will be the same for all
rect.props <- data.frame(lineage=character(), x0=numeric(), x1=numeric(),
                         y0=numeric(), y1=numeric(), col=character(), stringsAsFactors=FALSE);

# process df
# initialize to first entry
y0 <- yy[1];
cur.lin <- lin$lineage[1];
cnt <- 1; # counter
n <- length(lin[,1]);
for (i in 2:n) { # start at 2
    y.cur <- lin$yy[i];
    # if the next lineage does not match the current one, finish previous rectangle & start new one
    if (lin$lineage[i] != cur.lin) {
        coll <- lin.cols$col[which(lin.cols$lin == cur.lin)];
        # log rectangle
        rect.props[cnt,] <- c(cur.lin, x.min, x.max, y0, y.cur, coll);
        # reset parameters
        y0 <- y.cur;
        cur.lin <- lin$lineage[i];
        cnt <- cnt + 1;
    } else if (i == n) { # last entry, so close rectangle
        coll <- lin.cols$col[which(lin.cols$lin == cur.lin)];
        rect.props[cnt,] <- c(cur.lin, x.min, x.max, y0, y.cur, coll);
    }
}

# finally, make rectangles
for (i in 1:length(rect.props[,1])) {
    # need to set colour in rgb format to use transparency
    coll <- col2rgb(rect.props$col[i])[,1];
    rect(xleft=rect.props$x0[i], ybottom=rect.props$y0[i], xright=rect.props$x1[i],
         ytop=rect.props$y1[i], col=rgb(coll[1], coll[2], coll[3], alpha=125, maxColorValue=255), lwd=0);
}

legend('topright', lin.cols$lin, pt.bg=lin.cols$col, pch=21, bty="n", cex=1, title="Linajes",pt.cex=2)
