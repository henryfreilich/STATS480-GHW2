dat <- read.csv("California2006GOVfv.csv");
names(dat);
dim(dat);

N <- dim(dat)[1];
N;

n <- 1500;
Ns <- xtabs(~ cname, dat);
Ns;

# use 0.25 * Votes to approx frauds
efapprox <- 0.25 * dat$Votes;
# define strata using cumulative sqrt of freq
freq <- table(trunc(efapprox / 3));
freq;
sqrtfreq <- sqrt(table(trunc(efapprox / 3)));
cbind(freq, sqrtfreq);

# computation
cumsqrtfreq <- sqrtfreq;
for ( i in 2:length(sqrtfreq)) {
  cumsqrtfreq[i] <- cumsqrtfreq[i-1] + sqrtfreq[i];
}
cbind(freq, sqrtfreq, cumsqrtfreq);

nstrata <- 10;
max(cumsqrtfreq) / nstrata;
max(cumsqrtfreq) / nstrata * 1:nstrata;

cutpoints <- max(cumsqrtfreq) / nstrata * 1:nstrata;

cumstrata <- apply(outer(cumsqrtfreq, cutpoints, ">"), 1, sum) + 1;
cumstrata;
table(cumstrata);
cbind(freq, sqrtfreq, cumsqrtfreq, cumstrata);

mat <- cbind(freq, sqrtfreq, cumsqsrtfreq, cumstrata);

cstrata <- vector(length = N);
for (i in unique(cumstrata)) {
  idx <- trunc(efapprox / 3) %in%
    as.numeric(dimnames(mat)[[1]][mat[, "cumstrata" == i] == i]);
  cstrata[idx] <- i;
}
table(cstrata);

strata <- unique(cstrata);
Nsopt <- xtabs(~ cstrata, dat);
Nsopt;

sdevs <- sapply(strata, function(z) {
  sqrt(var(efapprox[cstrata == z]))
});
sdevs;

set.seed(881)
source("stallocm.R");
stallocm(Nsopt, sdevs, 5, 1)
stallocm(Nsopt, sdevs, .5, 1)
nsB5 <- round(stallocm(Nsopt, sdevs, .5, 1)$ni);
sum(nsB5 <- ifelse(nsB5 < 2, 2, nsB5));
nsB5;
sum(nsB5);

sstrata <- vector(mode = "character");
ef <- vector(mode = "numeric");
set.seed(881)
for(j in strata) {
  sstrata <- c(sstrata, rep(j, nsB5[j]));
  ef <- c(ef, dat$Nfraudmean[cstrata == j][sample(Nsopt[j] ,nsB5[j])]);
}
sdatB5 <- data.frame(sstrata, ef);

source("stmean.R");
STMB5 <- stmean(sdatB5, "ef", "sstrata", Nsopt);
STMB5$mean;
STMB5$bound;
sum(nsB5)
