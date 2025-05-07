#!/bin/bash
#set -x
set -o posix
#set -u
#set -e
#+
#
# ============
# Fmake_bld.sh
# ============
#
# --------------------
# Make build directory
# --------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ Fmake_bld.sh
#
#
# DESCRIPTION
# ===========
#
#
# Under CONFIG_NAME :
# - Make the build directory 
# - Create repositories needed :
# - BLD for compilation 
#
# A tmpdir can be specified for memory issues.
#
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./Fmake_bld.sh NEMOGCM/cfgs GYRE  /usr/tmp
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: Fmake_bld.sh 9651 2018-05-28 06:47:14Z nicolasmartin $
#
#
#
#   * creation
#
#-
[ ! -d ${2}/${1}/BLD ] && \mkdir -p ${2}/${1}/BLD
# enforce presence of cpp_tools.fcm (write a blank one if not present in the tools directory)
# cp instead of ln to avoid overwiting previous tool cpp_XXX.fcm file when compiling a file without cpp_YYY.fcm file.
[ -f ${2}/${1}/cpp_${1}.fcm ] && ln -sf -f ${2}/${1}/cpp_${1}.fcm ${2}/${1}/BLD/cpp_tools.fcm || echo 'bld::tool::fppkeys ' > ${2}/${1}/BLD/cpp_tools.fcm
