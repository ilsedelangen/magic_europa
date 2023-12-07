// This file was automatically generated by 'make' from file 'omp_SH_to_spat.gen.c'.
// To modify it, please consider modifying omp_SH_to_spat.gen.c
/*
 * Copyright (c) 2010-2019 Centre National de la Recherche Scientifique.
 * written by Nathanael Schaeffer (CNRS, ISTerre, Grenoble, France).
 * 
 * nathanael.schaeffer@univ-grenoble-alpes.fr
 * 
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software. You can use,
 * modify and/or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 * 
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 * 
 */


  #ifndef SHT_GRAD
	void GEN3(_sy2,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, cplx *Tlm, v2d *BtF, v2d *BpF, const long int llim, const unsigned im, int it0, int it1);
  #else
	void GEN3(_sy1s,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, v2d *BtF, v2d *BpF, const long int llim, const unsigned im, int it0, int it1);
  #endif

  #ifndef SHT_GRAD
	void GEN3(_sy2_hi,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, cplx *Tlm, v2d *BtF, v2d *BpF, const long int llim, const unsigned im, int it0, int it1);
  #else
	void GEN3(_sy1s_hi,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, v2d *BtF, v2d *BpF, const long int llim, const unsigned im, int it0, int it1);
  #endif




  #ifndef SHT_GRAD
	static void GEN3(SHsphtor_to_spat_omp_a,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, cplx *Tlm, double *Vt, double *Vp, long int llim) {
  #else
	static void GEN3(SHsph_to_spat_omp_a,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, double *Vt, double *Vp, long int llim) {
  #endif

	unsigned imlim = 0;
	v2d* BtF = (v2d*) Vt;	v2d* BpF = (v2d*) Vp;

  #ifndef SHT_AXISYM
	imlim = MTR;
	#ifdef SHT_VAR_LTR
		if (imlim*MRES > (unsigned) llim) imlim = ((unsigned) llim)/MRES;		// 32bit mul and div should be faster
	#endif
	if (shtns->fftc_mode > 0) {		// alloc memory for the FFT
		unsigned long nv = shtns->nspat;
		BtF = (v2d*) VMALLOC( 2*nv * sizeof(double) );
		BpF = BtF + nv/2;
	}
  #endif

  #pragma omp parallel num_threads(shtns->nthreads)
  {
	const int it0=0;
	const int it1=NLAT_2;
	#pragma omp for schedule(static,1) nowait
	for (int im=0; im<=imlim; im++)
	{
	#ifndef SHT_GRAD
		GEN3(_sy2_hi,NWAY,SUFFIX)(shtns, Slm, Tlm, BtF, BpF, llim, im, it0, it1);
	#else
		GEN3(_sy1s_hi,NWAY,SUFFIX)(shtns, Slm, BtF, BpF, llim, im, it0, it1);
	#endif
	}

  #ifndef SHT_AXISYM
	// padding for high m's
	if (NPHI-1 > 2*imlim) {
		const int m_inc = shtns->nlat_padded >> 1;
		#pragma omp for schedule(static) nowait
		for (int im=imlim+1; im < NPHI-imlim; im++)  {
			memset(BtF + m_inc*im, 0, sizeof(cplx)* m_inc );
			memset(BpF + m_inc*im, 0, sizeof(cplx)* m_inc );
		}
	}
  #endif
  }

  #ifndef SHT_AXISYM
    // NPHI > 1 as SHT_AXISYM is not defined.
	if (shtns->fftc_mode >= 0) {
		if (shtns->fftc_mode != 1) {
			fftw_execute_dft(shtns->ifftc, ((cplx *) BtF), ((cplx *) Vt));
			fftw_execute_dft(shtns->ifftc, ((cplx *) BpF), ((cplx *) Vp));
		} else {		// split dft
			fftw_execute_split_dft(shtns->ifftc,((double*)BtF)+1, ((double*)BtF), Vt+NPHI, Vt);
			fftw_execute_split_dft(shtns->ifftc,((double*)BpF)+1, ((double*)BpF), Vp+NPHI, Vp);
			VFREE(BtF);		// this frees also BpF.
		}
	}
  #endif
  }


  #ifndef SHT_GRAD
	static void GEN3(SHsphtor_to_spat_omp_b,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, cplx *Tlm, double *Vt, double *Vp, long int llim) {
  #else
	static void GEN3(SHsph_to_spat_omp_b,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Slm, double *Vt, double *Vp, long int llim) {
  #endif

	unsigned imlim = 0;
	v2d* BtF = (v2d*) Vt;	v2d* BpF = (v2d*) Vp;

  #ifndef SHT_AXISYM
	imlim = MTR;
	#ifdef SHT_VAR_LTR
		if (imlim*MRES > (unsigned) llim) imlim = ((unsigned) llim)/MRES;		// 32bit mul and div should be faster
	#endif
	if (shtns->fftc_mode > 0) {		// alloc memory for the FFT
		unsigned long nv = shtns->nspat;
		BtF = (v2d*) VMALLOC( 2*nv * sizeof(double) );
		BpF = BtF + nv/2;
	}
  #endif
  
  #pragma omp parallel num_threads(shtns->nthreads)
  {
	  int it0 = 0;
	  int it1 = NLAT_2;
	  const int it_step = NLAT_2 / shtns->nthreads;
	#pragma omp for schedule(dynamic,1) collapse(1) nowait
	//for (int it0=0; it0<NLAT_2; it0 += it_step)
	for (int im=0; im <= imlim/2; im++)
	{
		//it1 = it0 + it_step;
		#ifndef SHT_GRAD
		GEN3(_sy2_hi,NWAY,SUFFIX)(shtns, Slm, Tlm, BtF, BpF, llim, im, it0, it1);
		if (imlim-im > im) {
			GEN3(_sy2_hi,NWAY,SUFFIX)(shtns, Slm, Tlm, BtF, BpF, llim, imlim-im, it0, it1);
		}
		#else
		GEN3(_sy1s_hi,NWAY,SUFFIX)(shtns, Slm, BtF, BpF, llim, im, it0, it1);
		if (imlim-im > im) {
			GEN3(_sy1s_hi,NWAY,SUFFIX)(shtns, Slm, BtF, BpF, llim, imlim-im, it0, it1);
		}
		#endif
	}

  #ifndef SHT_AXISYM
	// padding for high m's
	if (NPHI-1 > 2*imlim) {
		#pragma omp for schedule(dynamic) nowait
		for (int im=imlim+1; im < NPHI-imlim; im++)  {
			memset(BtF + NLAT_2*im, 0, sizeof(cplx)* NLAT_2 );
			memset(BpF + NLAT_2*im, 0, sizeof(cplx)* NLAT_2 );
		}
	}

	if (shtns->fftc_mode >= 0) {
		const int nblk = (NLAT/2) / shtns->nthreads;
		#pragma omp barrier
		if (shtns->fftc_mode != 1) {
			for (int k=0; k<shtns->nthreads; k++) {
				#pragma omp single nowait
				fftw_execute_dft(shtns->ifftc_block, ((cplx *) BtF) + k*nblk, ((cplx *) Vt) + k*nblk);
				#pragma omp single nowait
				fftw_execute_dft(shtns->ifftc_block, ((cplx *) BpF) + k*nblk, ((cplx *) Vp) + k*nblk);
			}
		} else {
			for (int k=0; k<shtns->nthreads; k++) {
				#pragma omp single nowait
				fftw_execute_split_dft(shtns->ifftc_block,((double*)BtF)+1 +2*k*nblk, ((double*)BtF) +2*k*nblk, Vt+NPHI*(1+2*k*nblk), Vt +NPHI*2*k*nblk);
				#pragma omp single nowait
				fftw_execute_split_dft(shtns->ifftc_block,((double*)BpF)+1 +2*k*nblk, ((double*)BpF) +2*k*nblk, Vp+NPHI*(1+2*k*nblk), Vp +NPHI*2*k*nblk);
			}
		}
	}
  #endif
  }

  #ifndef SHT_AXISYM
	if (shtns->fftc_mode > 0) {
			VFREE(BtF);		// this frees also BpF.
	}
  #endif

  }
