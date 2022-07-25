#!/bin/bash

maxima -r 'load("ms-vlasov-header.mac");quit();'

maxima -r 'load("ms-fpo-vlasov-diff-vol.mac");quit();'
maxima -r 'load("ms-fpo-vlasov-diff-surf.mac");quit();'
