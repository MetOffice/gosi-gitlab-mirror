# ======================================================================
#                    ***  sette/sette_error.py  ***
# NEMO SETTE error handling
# ======================================================================
# History : 5.1  !  2026-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------
#
"""NEMO SETTE error handling"""

class SetteError(Exception):
    """Handling of generic SETTE errors"""

    # Error message and type
    _error_message = ''
    _error_type = ''

    def __init__(self, error_message, error_type=''):
        """Initialisation"""
        super().__init__(error_message)
        self._error_message = error_message
        self._error_type = error_type

    def __str__(self):
        """Formatted error message"""
        if len(self._error_type) > 0:
            return ' '.join(['SETTE', self._error_type, 'error:',
                self._error_message])
        else:
            return 'SETTE error: '+self._error_message

class SetteErrorIdentifier(SetteError):
    """Handling of errors related to SETTE identifiers"""

    def __init__(self, message, identifier_type):
        """Initialisation"""
        error_type = 'identifier ('+identifier_type+')'
        super().__init__(message, error_type)

class SetteErrorPath(SetteError):
    """Handling of errors related to SETTE paths"""

    def __init__(self, message):
        """Initialisation"""
        super().__init__(message, 'path')
