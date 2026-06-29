#! /usr/bin/python
# ======================================================================
#                 ***  psct-LocalToGlobalArrays.py  ***
# ======================================================================
#  History : 5.0  !  2024  (S. Mueller)
#            5.0  !  2025-01  (S. Mueller) Update for compatibility with the latest PSyclone release version
#            5.1. !  2026-06  (S. Mueller) Adjustment for PSyclone-3.2.2 compatibility
# ----------------------------------------------------------------------
#
# PSyclone transformation script for the promotion of some local arrays to
# module scope. PSyclone provides this functionality through its
# HoistLocalArraysTrans transformation
# (https://psyclone.readthedocs.io/en/stable/transformations.html#available-transformations),
# which is applied to most procedures.
#
# ----------------------------------------------------------------------
# NEMO 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------

from psyclone.psyir.transformations import HoistLocalArraysTrans

# For PSyclone-3 compatibility
from psct_utils import P3APICompat

# ----------------------------------------------------------------------
# Rejection of modules and invokes (all names in lowercase)
# ----------------------------------------------------------------------
#
# Reject for compatibility with PSyclone 2.5.0
MODULES_REJECT = [ 'agrif2model', 'agrif_user' ]
# Invokes for which local arrays should not be promoted
INVOKES_REJECT = [ 'dia_dct' ]   # Issue with RESHAPE operation

# ----------------------------------------------------------------------
#              ***  PSyclone transformation procedure  ***
# ----------------------------------------------------------------------
def trans(psy):

    # For compatibility with PSyclone release version 3.0.0
    psy = P3APICompat(psy)

    if not len([ m for m in MODULES_REJECT if psy.name.lower()=='psy_'+m+'_psy' ]):
        for invoke in psy.invokes.invoke_list:
            if not invoke.name.lower() in INVOKES_REJECT:
                # Avoid the processing of procedures with the
                # 'ELEMENTAL' attribute
                if not invoke.schedule.symbol.is_elemental:
                    HoistLocalArraysTrans().apply(invoke.schedule)

    return
