module force_average

    use calc_ave
    use radial_data, only: nRstart, nRstop
    use truncation, only: n_phi_max, nlat_padded

    implicit none

    type(ave) :: LFr_ave, LFt_ave, LFp_ave

    LFr_ave%init(nRstop-nRstart+1,nlat_padded,n_phi_max)
    LFt_ave%init(nRstop-nRstart+1,nlat_padded,n_phi_max)
    LFp_ave%init(nRstop-nRstart+1,nlat_padded,n_phi_max)


end module force_average










