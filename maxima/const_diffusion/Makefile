BASISNAMES=["Ser", "Tensor"]
MAX_POLY_ORDER=2
MAX_NDIM=1
PREFIX=~/max-out

all:
	maxima -r 'basisNames : $(BASISNAMES);maxPolyOrder : $(MAX_POLY_ORDER); maxNdim : $(MAX_NDIM); outFn : "$(PREFIX)"; load("surf.mac");quit();'
clean:
	rm $(PREFIX)/*.c
