#!/bin/bash
maxima -r 'load("recovery-calc/recovery-unit")$testRecov("check")$values;quit();'
