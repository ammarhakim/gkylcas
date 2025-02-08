#!/bin/bash

maxima -r 'load("ms-fpo-vlasov-header.mac");quit();'

maxima -r 'load("ms-fpo-drag-coeff.mac");quit();'
maxima -r 'load("ms-fpo-diff-coeff.mac");quit();'
maxima -r 'load("ms-fpo-diff-coeff-surf.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-coeff-correct.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-drag-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-drag-surf.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-drag-boundary-surf.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-diff-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-diff-surf.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-diff-boundary-surf.mac");quit();'

maxima -r 'load("ms-moments-vlasov-fpo.mac");quit();'
maxima -r 'load("ms-moments-vlasov-fpo-header.mac");quit();'
