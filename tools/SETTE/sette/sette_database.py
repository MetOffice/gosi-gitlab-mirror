# ======================================================================
#                   ***  sette/sette_database.py  ***
# NEMO SETTE-validation-database access
# ======================================================================
# History : 5.1  !  2026-06  (S. Mueller) Initial version
# ----------------------------------------------------------------------
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE)
# ----------------------------------------------------------------------
#
"""NEMO SETTE-validation-database access

High-level functionality for accessing information stored in SETTE
validation databases
"""

from sette.sette_record import SetteRecord
from sette.sette_identifier import SetteIdentifierRevision
from sette.sette_identifier import SetteIdentifierVariant
from sette.sette_identifier import SetteIdentifierConfiguration
from sette.sette_identifier import SetteIdentifierTestrun
from sette.sette_identifier import SetteIdentifierFile
from sette.sette_identifier import SetteIdentifierCompiler
from sette.sette_identifier import SetteIdentifierTransform
from sette.sette_identifier import SetteIdentifierControl

class SetteDatabase():
    """NEMO SETTE-validation-database access

    Handling of a SETTE validation-database location and identifier
    constraints
    """

    # Base path to the database
    _base = None

    # Pointer to the current location inside the database
    _pointer = None

    # Identifier constraints
    _constraints = [None, None, None, None, None]

    # Metadata constraints
    _constraints_extra = dict()

    def __init__(self, base):
        """Initialise SETTE-validation-database access"""
        self._pointer = SetteRecord(base)
        self._constraints = [SetteIdentifierRevision(),
                SetteIdentifierVariant(), SetteIdentifierConfiguration(),
                SetteIdentifierTestrun(), SetteIdentifierFile()]

    @property
    def pointer(self):
        """Record pointer"""
        return self._pointer

    @property
    def revision(self):
        """Revision identifier"""
        return self._constraints[0].value

    @revision.setter
    def revision(self, identifier):
        """Revision identifier"""
        self._constraints[0].value = identifier

    @revision.deleter
    def revision(self):
        """Revision identifier"""
        del self._constraints[0].value

    @property
    def variant(self):
        """Variant identifier"""
        return self._constraints[1].value

    @variant.setter
    def variant(self, identifier):
        """Variant identifier"""
        self._constraints[1].value = identifier

    @variant.deleter
    def variant(self):
        """Variant identifier"""
        del self._constraints[1].value

    @property
    def configuration(self):
        """Configuration identifier"""
        return self._constraints[2].value

    @configuration.setter
    def configuration(self, identifier):
        """Configuration identifier"""
        self._constraints[2].value = identifier

    @configuration.deleter
    def configuration(self):
        """Configuration identifier"""
        del self._constraints[2].value

    @property
    def testrun(self):
        """Test-run identifier"""
        return self._constraints[3].value

    @testrun.setter
    def testrun(self, identifier):
        """Test-run identifier"""
        self._constraints[3].value = identifier

    @testrun.deleter
    def testrun(self):
        """Test-run identifier"""
        del self._constraints[3].value

    @property
    def file(self):
        """File identifier"""
        return self._constraints[4].value

    @file.setter
    def file(self, identifier):
        """File identifier"""
        self._constraints[4].value = identifier

    @file.deleter
    def file(self):
        """File identifier"""
        del self._constraints[4].value

    def set_extra_constraint(self, identifier, key, value):
        """Set extra constraint"""
        if not identifier in self._constraints_extra.keys():
            self._constraints_extra[identifier] = dict()
        if identifier == 'variant':
            val = None
            if key == 'COMPILER':
                val = SetteIdentifierCompiler(value=value)
            if key == 'TRANSFORM':
                val = SetteIdentifierTransform(value=value)
            if key.startswith('USING_'):
                val = SetteIdentifierControl(value=value)
            if val is not None:
                self._constraints_extra[identifier][key] = val

    def focus(self):
        """Focus the pointer in order to absorb some constraints"""
        for n in range(self.pointer.depth, len(self._constraints)):
            if self._constraints[n].value is not None:
                if self.pointer.depth == n:
                    try:
                        self.pointer.focus(self._constraints[n].value)
                    except:
                        pass

    def unfocus(self):
        """Reset pointer to the database root"""
        self.pointer.reset()

    def reset(self):
        """Reset pointer and remove constraints"""
        self.unfocus()
        del self.revision
        del self.variant
        del self.configuration
        del self.testrun
        del self.file

    def list_constrained(self):
        """Identifier list after application of restrictions"""
        entries = self.pointer.list()
        # Apply identifier restrictions
        if self._constraints[self.pointer.depth].value is not None:
            if self._constraints[self.pointer.depth].value in entries:
                entries = [self._constraints[self.pointer.depth].value]
            else:
                entries = []
        depth = self.pointer.depth
        depth_type = self.pointer.depth_type
        # Apply metadata restrictions
        if depth_type in self._constraints_extra.keys() and len(
                self._constraints_extra[depth_type]) > 0:
            entries_constrained = []
            for entry in entries:
                # Temporarily add identifier to path to trigger metadata
                # retrieval
                self.pointer.focus(entry)
                # Include entry by default
                entries_constrained.append(entry)
                # Remove entry if a restriction applies
                for key in self.pointer._identifiers[depth].lookup.keys():
                    val = self.pointer._identifiers[depth].lookup[key].value
                    if key in self._constraints_extra[depth_type].keys():
                        if entry in entries_constrained:
                            v = self._constraints_extra[depth_type][key].value
                            if val != v:
                                entries_constrained.remove(entry)
                # Remove identifier
                self.pointer.blur()
            entries = entries_constrained
        # Return reduced list
        return entries

class SetteDatabaseView(SetteDatabase):
    """NEMO SETTE-validation-database catalogue search

    The SETTE-validation-database catalogue (all available paths) can be
    narrowed through identifier constraints (fixed identifier values);
    the structure of the resultant catalogue subset can be visualised
    and groups of identical identifiers be counted.
    """

    def show(self, nmax):
        """Display the database structure from the current pointer

        The database subtree that spans 'nmax' layers from the current
        pointer and is subject to the current constraints is shown
        """
        # Display database subtree
        identifiers = self._show(nmax)
        # Display variant metadata (if any)
        if 'variant' in identifiers['lookup'].keys():
            variants = identifiers['lookup']['variant']
            # Collate all metadata keys and the required field lengths
            all_keys = dict()
            for variant in variants.keys():
                for key in variants[variant].keys():
                    if key not in all_keys.keys():
                        all_keys[key] = len(key.replace('USING_',''))
                    all_keys[key] = max(all_keys[key],
                            len(variants[variant][key].value))
            if len(all_keys):
                labels = ['Legend (variants)']
                fmts = ['{:<32}']
                sep = '='*(sum(all_keys.values())+len(all_keys)*3+32)
                for key in all_keys.keys():
                    labels += [key.replace('USING_','')]
                    fmts += ['{:>'+str(all_keys[key])+'}']
                print()
                print(sep)
                print(' | '.join(fmts).replace('>','^').format(*labels))
                print(sep)
                for variant in variants.keys():
                    row = [variant]
                    for key in all_keys.keys():
                        if key in variants[variant].keys():
                            row.append(variants[variant][key].value)
                        else:
                            row.append('')
                    print(' | '.join(fmts).format(*row))
        return identifiers

    def _show(self, nmax, identifiers=None, line='', line_blank=''):
        """Recursive function for displaying database subtrees"""
        # Cap the maximum depth
        nmax = min(len(self._constraints), nmax)
        # Current database depth
        n = self.pointer.depth
        # Get the list of identifiers available after the application
        # of constraints
        entries = self.list_constrained()
        # Get the entry type at the current location
        entry_type = self.pointer.depth_type
        # Header titles and field widths
        column_titles = [i.identifier_type.upper() for i in
                self.pointer._identifiers]
        widths = [i.len_max for i in self.pointer._identifiers]
        # Adjust the width of the revision column (if any)
        if entry_type == 'revision':
            widths[0] = max([len(column_titles[0])]+[len(r) for r in entries])
        # Column formats
        fmts = ['{:>'+str(w)+'}' for w in widths]
        # Create an empty entry if the path does not continue
        if len(entries) == 0:
            entries = ['']
        # Initialise counters and display the column labels during the
        # initial function call
        if identifiers is None:
            # Nothing to display
            if nmax <= n:
                return dict()
            # Initialise identifier counters and metadata storage
            identifiers = {'amount': dict(), 'lookup': dict()}
            # Display column labels
            print('='*(sum(widths[n:nmax])+3*(nmax-n)))
            print('   '+' | '.join(fmts[n:nmax]).replace('>', '^').
                    format(*column_titles[n:nmax]))
            print('='*(sum(widths[n:nmax])+3*(nmax-n)))
        # Initialise identifier counters and metadata storage for the
        # current entry type
        for category in ['amount', 'lookup']:
            if not entry_type in identifiers[category].keys():
                identifiers[category][entry_type] = dict()
        # Print entries
        s = ['-', '-', '+', '|']
        for entry in entries:
            # Group and count identifiers in the local set
            if len(entry) > 0:
                if not entry in identifiers['amount'][entry_type].keys():
                    identifiers['amount'][entry_type][entry] = 1
                else:
                    identifiers['amount'][entry_type][entry] += 1
            # Final identifier of local set
            if entry == entries[-1]:
                if entry == entries[0]:
                    s[2:] = ['-', ' ']
                else:
                    s[2:] = ['\\', ' ']
            # No identifier in local set
            if len(entry) == 0:
                s = [' ']*4
            # Add next segment to the path
            line += s[0]+s[2]+s[1]+fmts[n].format(entry).replace(' ', s[1])
            # Follow the path or, if complete, output it (and group and
            # count the last identifier)
            if len(entry) > 0 and n < nmax-1:
                self.pointer.focus(entry)
                # Record metadata (if any)
                lookup = self.pointer._identifiers[n].lookup
                identifiers['lookup'][entry_type][entry] = lookup
                identifiers = self._show(nmax, identifiers, line,
                        ' '.join([line_blank, s[3], fmts[n].format('')]))
                self.pointer.blur()
            else:
                print(line)
            # Prepare next output line
            line = line_blank
            s[0] = ' '
        return identifiers
