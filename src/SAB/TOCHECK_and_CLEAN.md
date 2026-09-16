DIUA: Why it is needed, iom need it in the module with a ONLY ln_diur* but never use this variable ???? +> we should be able to clean it
ICE: we assume SAB eat already the dynamical SSH, so if we set with a cpp_key the call to ssh_dynvar only if not SAB we could remove some dependency I think
SBC: move sbc_oce definition needed to a sab_oce.F90
