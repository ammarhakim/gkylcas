#!/bin/bash

# Drag Coeff and Sign
maxima -b ms-fpo-drag-coeff.mac

# Diffusion tensor
maxima -b ms-fpo-diff-coeff.mac

# Diff coeff surf
maxima -b ms-fpo-diff-coeff-surf.mac

# Conservation Moments
maxima -b ms-moments-vlasov-fpo.mac

# Conservation corrections
maxima -b ms-fpo-vlasov-coeff-correct.mac

# Drag terms
maxima -b ms-fpo-vlasov-drag-vol.mac
maxima -b ms-fpo-vlasov-drag-surf.mac
maxima -b ms-fpo-vlasov-drag-boundary-surf.mac

# Diffusion terms
maxima -b ms-fpo-vlasov-diff-vol.mac
maxima -b ms-fpo-vlasov-diff-surf.mac
maxima -b ms-fpo-vlasov-diff-boundar-surf.mac

# Headers
maxima -b ms-fpo-vlasov-header.mac
maxima -b ms-moments-vlasov-fpo-header.mac

