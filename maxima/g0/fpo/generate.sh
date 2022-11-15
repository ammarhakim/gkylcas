#!/bin/bash

maxima -r 'load("ms-fpo-vlasov-header.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-drag-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-drag-surf.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-drag-boundary-surf.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-diff-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-diff-surf.mac");quit();'
#maxima -r 'load("ms-fpo-vlasov-diff-boundary-surf.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-D-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-D-surf.mac");quit();'
#maxima -r 'load("ms-fpo-vlasov-D-boundary-surf.mac");quit();'
