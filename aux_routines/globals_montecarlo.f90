module globals_montecarlo
  use prec; use parameters

  integer, parameter:: nff=1
  integer, parameter:: nir=1
  integer, parameter:: ngg=1
  integer, parameter:: nss=1
  integer, parameter:: nmoments=7
  integer, parameter:: nmomentsall=25
  integer, parameter:: ninit = 5000
  integer, parameter:: n_save = 28

  type estimation
     integer iffmax,iggmax
     real(dp) grid_ff(nff), min_ff, max_ff, true_ff
     real(dp) grid_ir(nir), min_ir, max_ir, true_ir
     real(dp) grid_gg(ngg), min_gg, max_gg, true_gg
     real(dp) grid_ss(nss), min_ss, max_ss, true_ss
     real(dp) moments_data(n_save,1)
  end type estimation

  type(estimation) smm
end module globals_montecarlo
