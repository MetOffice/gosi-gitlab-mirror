# ======================================================================
#                  ***  sette/sette_identifier.py  ***
# NEMO SETTE-validation-database identifiers
# ======================================================================
# History : 5.1  !  2026-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------
#
"""NEMO SETTE-validation-database identifiers

The SETTE validation database uses a four-tier directory structure where
individual records are locatable using a file path relative to the
database root that is formed of five identifiers (<revision>, <variant>,
<configuration>, <test run>, <file>), each of which adhers to a
specified format. This module provides a generic class to facilitate
the handling of such identifiers, as well as five subclasses for
validating identifiers of each of the respective five identifier types.
"""

import re
import pathlib
from sette.sette_error import SetteErrorIdentifier, SetteErrorPath

class SetteIdentifier():
    """Generic validated SETTE identifier"""

    # Identifier value, base path, and, if a base path is specified, a
    # flag to indicate the existence of the identifier
    _value = None
    _path = None
    exists = False

    # Description, maximum length, type, and patterns used to validate
    # the identifier value
    description = 'string, 1-128 characters'
    len_max = 128
    len_min = 1
    identifier_type = 'generic'
    validation_patterns = []

    # Dictionary for storing metadata associated with the identifier
    # value
    _lookup = None

    def __init__(self, value=None, path=None):
        """Initialisation and, if provided, validate the identifier"""
        if value is not None:
            self.value = value
        if path is not None:
            self.path = path
        # Initialise lookup-data dictionary
        self._lookup = dict()

    @property
    def value(self):
        """Identifier value"""
        return self._value

    @value.setter
    def value(self, value):
        """Identifier value"""
        if self.validate(value):
            self._value = value
            self.update()
        else:
            if len(self.description):
                description = '('+self.description+' expected)'
            raise SetteErrorIdentifier('incompatible with expected format ('+
                    self.description+')', self.identifier_type)

    @value.deleter
    def value(self):
        """Identifier value"""
        self._value = None
        del self.path
        self.update()

    @property
    def path(self):
        """Identifier path"""
        return self._path

    @path.setter
    def path(self, path):
        """Identifier path"""
        if isinstance(path, str):
            path = pathlib.Path(path)
        if isinstance(path, pathlib.Path):
            self._path = path
            self.update()
        else:
            raise SetteErrorPath('unknown path type')

    @path.deleter
    def path(self):
        """Identifier path"""
        self._path = None
        self.update()

    def update(self):
        """Update existence flag and look-up data"""
        # Check existence of the identifier if a path is available
        if self.path is not None and self.value is not None:
            self.exists = self.path.joinpath(self.value).is_dir()
            self.exists |= self.path.joinpath(self.value).is_file()
        else:
            self.exists = False
        # Reset look-up data
        self._lookup = dict()

    @property
    def lookup(self):
        """Return metadata associated with the identifier"""
        return self._lookup

    def validate(self, value):
        """Validate identifier"""
        if isinstance(value, str):
            if len(value) < self.len_min or len(value) > self.len_max:
                return False
            patterns = []
            for p in self.validation_patterns:
                patterns.append(re.compile('^'+p+'$'))
            if len(patterns) > 0:
                return any([p.fullmatch(value) is not None for p in patterns])
            else:
                return True
        return False

class SetteIdentifierRevision(SetteIdentifier):
    """Validated SETTE revision identifier"""

    validation_patterns = ['[0-9a-fA-F]{8,40}[+]?']
    identifier_type = 'revision'
    description = 'hexadecimal, 8-40 digits, optional suffix \'+\''
    len_max = 41

class SetteIdentifierVariant(SetteIdentifier):
    """Validated SETTE variant identifier"""

    validation_patterns = ['[0-9a-fA-F]{32}']
    identifier_type = 'variant'
    description = 'hexadecimal, 32 digits'
    len_max = 32

    def update(self):
        """Update existence flag and metadata"""
        super().update()
        # Check existence of metadata file if a path is available
        if self.path is not None and self.value is not None:
            if not self.path.joinpath(self.value+'.lookup').is_file():
                self.exists = False
        # Additional metadata
        if self.exists:
            lookup_raw = pathlib.Path(self.path,
                    self.value+'.lookup').read_text()
            for entry in lookup_raw.strip('"\n').split('";"'):
                (key,val) = entry.split('","')
                if key == 'COMPILER':
                    self._lookup[key] = SetteIdentifierCompiler(value=val)
                if key == 'TRANSFORM':
                    self._lookup[key] = SetteIdentifierTransform(value=val)
                if key.startswith('USING_'):
                    self._lookup[key] = SetteIdentifierControl(value=val)

class SetteIdentifierConfiguration(SetteIdentifier):
    """Validated SETTE configuration identifier"""

    validation_patterns = ['AGRIF_DEMO', 'AGRIF_DEMO_NOAGRIF', 'AMM12',
            'C1D_PAPA', 'ICE_AGRIF', 'GYRE_GO', 'GYRE_PISCES', 'ISOMIP+',
            'IWAVE', 'LOCK_EXCHANGE', 'ORCA2_ICE_PISCES', 'ORCA2_ICE_PISCES',
            'ORCA2_OFF_PISCES', 'ORCA2_ICE_OBS', 'ORCA2_ICE_OBS',
            'ORCA2_ICE_OBS_SAO', 'ORCA2_SAS_ICE', 'OVERFLOW', 'SWG', 'VORTEX',
            'WED025']
    identifier_type = 'configuration'
    description = ', '.join(validation_patterns[:-1])
    description += ', or '+validation_patterns[-1]
    len_max = 17

class SetteIdentifierTestrun(SetteIdentifier):
    """Validate SETTE test-run identifier"""

    validation_patterns = ['EXP-[a-zA-Z0-9]{1,25}', 'MPP', 'MPPREF', 'NOAGRIF',
            'REF', 'ROT_0[09]0', 'ROT_180', 'RST', 'SAO', 'SAOREF']
    identifier_type = 'testrun'
    description = 'MPP, MPPREF, NOAGRIF, REF, ROT_000, ROT_090, ROT_180, RST, '
    description += 'SAO, SAOREF, or a name starting in EXP-'
    len_max = 30

class SetteIdentifierFile(SetteIdentifier):
    """Validated SETTE test-run output file identifier"""

    validation_patterns = ['ocean.output', 'run.stat', 'tracer.stat',
            'obs.stat', 'timing.output']
    identifier_type = 'file'
    description = ', '.join(validation_patterns[:-1])+', or '
    description += validation_patterns[-1]
    len_max = 13

class SetteIdentifierCompiler(SetteIdentifier):
    """Compiler metadata associated with the variant identifier"""

    validation_patterns = ['[0-9a-zA-Z.+_-]{1,64}']
    identifier_type = 'variant_compiler'
    description = "alphanumeric or in {'.', '+', '_', '-'}, 1 to 64 "
    description += "characters"
    len_max = 64

class SetteIdentifierTransform(SetteIdentifier):
    """Transform metadata associated with the variant identifier"""

    validation_patterns = ['[0-9a-zA-Z]{0,64}']
    identifier_type = 'variant_transform'
    description = 'alphanumeric, 0 to 64 characters'
    len_max = 64
    len_min = 0

class SetteIdentifierControl(SetteIdentifier):
    """Control metadata associated with the variant identifier"""

    validation_patterns = ['yes', 'no']
    identifier_type = 'variant_control'
    description = "'yes' or 'no'"
    len_max = 3
