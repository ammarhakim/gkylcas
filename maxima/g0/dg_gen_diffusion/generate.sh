#!/bin/bash

maxima -r 'load("diff-vol-2x-C.mac");quit();'
maxima -r 'load("diff-vol-3x-C.mac");quit();'

maxima -r 'load("diff-surfx-2x-C.mac");quit();'
maxima -r 'load("diff-surfy-2x-C.mac");quit();'

maxima -r 'load("diff-surfx-3x-C.mac");quit();'
maxima -r 'load("diff-surfy-3x-C.mac");quit();'
maxima -r 'load("diff-surfz-3x-C.mac");quit();'
