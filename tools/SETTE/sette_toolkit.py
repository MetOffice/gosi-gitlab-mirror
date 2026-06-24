#! /usr/bin/env python3
# ======================================================================
#                       ***  sette_toolkit.py  ***
# NEMO SETTE toolkit
# ======================================================================
# History : 5.1  !  2026-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------
#
"""NEMO SETTE toolkit

The NEMO SETTE toolkit interface accepts one command:
    'catalogue' - SETTE validation-database catalogue enquiry
"""

import argparse
import pathlib
from sette.sette_database import SetteDatabaseView
from sette.sette_identifier import SetteIdentifierRevision
from sette.sette_identifier import SetteIdentifierVariant
from sette.sette_identifier import SetteIdentifierConfiguration
from sette.sette_identifier import SetteIdentifierTestrun
from sette.sette_identifier import SetteIdentifierFile
from sette.sette_identifier import SetteIdentifierCompiler
from sette.sette_identifier import SetteIdentifierTransform
from sette.sette_identifier import SetteIdentifierControl

if __name__ == '__main__':

    # SETTE toolkit command-line interface
    argp = argparse.ArgumentParser(prog='sette_toolkit.py',
            description='The NEMO SETTE toolkit')
    # Path to the SETTE validation database
    argp.add_argument('--path', help='SETTE validation-database root',
            default='.', type=pathlib.Path)
    # Command 'catalogue'
    argsp = argp.add_subparsers(required=True, dest='command')
    argp_cat = argsp.add_parser('catalogue',
            help='Display the SETTE validation-database catalogue')
    # Maximum database depth
    argp_cat.add_argument('--depth', type=int, choices=range(6), default=3,
            required=False, help='Maximum database depth (0-5)')
    # Identifier constraints
    argp_cat.add_argument('--revision', help='Revision-identifier constraint',
            default=None, type=SetteIdentifierRevision)
    argp_cat.add_argument('--variant', help='Variant-identifier constraint',
            default=None, type=SetteIdentifierVariant)
    argp_cat.add_argument('--configuration',
            help='Configuration-identifier constraint',
            default=None, type=SetteIdentifierConfiguration)
    argp_cat.add_argument('--testrun', help='Test-run-identifier constraint',
            default=None, type=SetteIdentifierTestrun)
    argp_cat.add_argument('--file', help='File-identifier constraint',
            default=None, type=SetteIdentifierFile)
    # Additional optional constraints (variant specifics)
    argp_cat.add_argument('--compiler',
            help='Compilation environment variant-identifier constraint',
            default=None, type=SetteIdentifierCompiler)
    argp_cat.add_argument('--transform',
            help='Source-to-source transformation variant-identifier'+
            ' constraint', default=None, type=SetteIdentifierTransform)
    argp_cat.add_argument('--qco', help='USING_QCO control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--si3_1d', help='USING_SI3_1D control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--xios', help='USING_XIOS control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--debug', help='USING_DEBUG control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--timing', help='USING_TIMING control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--icebergs', help='USING_ICEBERGS control '+
            'constraint', default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--abl', help='USING_ABL control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--extra_halo', help='USING_EXTRA_HALO control '+
            'constraint', default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--collectives', help='USING_COLLECTIVES control '+
            'constraint', default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--nnogather', help='USING_NNOGATHER control '+
            'constraint', default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--tiling', help='USING_TILING control constraint',
            default=None, type=SetteIdentifierControl)
    argp_cat.add_argument('--mpmd', help='USING_MPMD control constraint',
            default=None, type=SetteIdentifierControl)
    # Parse command-line arguments
    arg = argp.parse_args()

    # Display validation-database catalogue subtree
    if arg.command == 'catalogue':
        catalogue = SetteDatabaseView(arg.path)
        # Apply constraints
        if arg.revision is not None:
            catalogue.revision = arg.revision.value
        if arg.variant is not None:
            catalogue.variant = arg.variant.value
        if arg.configuration is not None:
            catalogue.configuration = arg.configuration.value
        if arg.testrun is not None:
            catalogue.testrun = arg.testrun.value
        if arg.file is not None:
            catalogue.file = arg.file.value
        if arg.compiler is not None:
            catalogue.set_extra_constraint('variant', 'COMPILER',
                    arg.compiler.value)
        if arg.transform is not None:
            catalogue.set_extra_constraint('variant', 'TRANSFORM',
                    arg.transform.value)
        if arg.qco is not None:
            catalogue.set_extra_constraint('variant', 'USING_QCO',
                    arg.qco.value)
        if arg.si3_1d is not None:
            catalogue.set_extra_constraint('variant', 'USING_SI3_1D',
                    arg.si3_1d.value)
        if arg.xios is not None:
            catalogue.set_extra_constraint('variant', 'USING_XIOS',
                    arg.xios.value)
        if arg.debug is not None:
            catalogue.set_extra_constraint('variant', 'USING_DEBUG',
                    arg.debug.value)
        if arg.timing is not None:
            catalogue.set_extra_constraint('variant', 'USING_TIMING',
                    arg.timing.value)
        if arg.icebergs is not None:
            catalogue.set_extra_constraint('variant', 'USING_ICEBERGS',
                    arg.icebergs.value)
        if arg.abl is not None:
            catalogue.set_extra_constraint('variant', 'USING_ABL',
                    arg.abl.value)
        if arg.extra_halo is not None:
            catalogue.set_extra_constraint('variant', 'USING_EXTRA_HALO',
                    arg.extra_halo.value)
        if arg.collectives is not None:
            catalogue.set_extra_constraint('variant', 'USING_COLLECTIVES',
                    arg.collectives.value)
        if arg.nnogather is not None:
            catalogue.set_extra_constraint('variant', 'USING_NNOGATHER',
                    arg.nnogather.value)
        if arg.tiling is not None:
            catalogue.set_extra_constraint('variant', 'USING_TILING',
                    arg.tiling.value)
        if arg.mpmd is not None:
            catalogue.set_extra_constraint('variant', 'USING_MPMD',
                    arg.mpmd.value)
        catalogue.show(arg.depth)
