// This file was automatically generated by 'make' from file 'kernel_SH_to_spat.gen.c'.
// To modify it, please consider modifying kernel_SH_to_spat.gen.c
/*
 * Copyright (c) 2010-2021 Centre National de la Recherche Scientifique.
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

//////////////////////////////////////////////////

	#ifdef HI_LLIM
	#define BASE _sy1_hi
	#ifndef SHT_GRAD
	#else
	#endif
	#else
	#define BASE _sy1
	#ifndef SHT_GRAD
	#else
	#endif
	#endif

	void GEN3(BASE,NWAY,SUFFIX)(shtns_cfg shtns, cplx *Qlm, v2d *BrF, long int llim, const unsigned im, int it0, int it1) {
  #ifndef SHT_GRAD
  #else
  #endif

	#if !defined( _GCC_VEC_ ) && (NWAY & 1)
	#error "NWAY must be even when compiled without explicit vectorization."
	#endif
	#if VSIZE2*NWAY > 32
	#error "VSIZE2*NWAY must not exceed 32"
	#endif

  #ifndef SHT_AXISYM
   #ifndef SHTNS_ISHIOKA
	#define qr(l) vall(creal(Ql[l]))
	#define qi(l) vall(cimag(Ql[l]))
   #else
	#define qr(l) vall( ((double*) QQl)[2*(l)]   )
	#define qi(l) vall( ((double*) QQl)[2*(l)+1] )
   #endif
  #endif
	long int nk,k,l,m;
	double *alm, *al;
	double *ct, *st;
	double Ql0[llim+2];
  #ifdef SHTNS_ISHIOKA
	v2d QQl[llim+2];
  #endif

	ct = shtns->ct;		st = shtns->st;
	nk = it1;	//NLAT_2;
	#if _GCC_VEC_
		nk = ((unsigned)(nk+VSIZE2-1)) / VSIZE2;
		it0 = ((unsigned)(it0+VSIZE2-1)) / VSIZE2;
	#endif

	if (im == 0)
	{	//	im=0;
		#ifdef SHT_GRAD
			// TODO: fix k,nk bounds
		#endif
 		l=1;
		alm = shtns->alm;
		Ql0[0] = (double) Qlm[0];		// l=0
		do {		// for m=0, compress the complex Q,S,T to double
			Ql0[l] = creal( Qlm[l] );	//	Ql[l+1] = (double) Qlm[l+1];
			++l;
		} while(l<=llim);
		k=it0;
		do {
			l=0;	al = alm;
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
			rnd re[NWAY], ro[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(ct, j+k);
				y0[j] = vall(al[0]);
				re[j] = y0[j] * vall(Ql0[0]);
			}
			for (int j=0; j<NWAY; ++j) {
				y1[j]  = vall(al[0]*al[1]) * cost[j];
			}
			for (int j=0; j<NWAY; ++j) {
				ro[j] = y1[j] * vall(Ql0[1]);
			}
			al+=2;	l+=2;
			while(l<llim) {
				for (int j=0; j<NWAY; ++j) {
					y0[j]  = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
					re[j] += y0[j] * vall(Ql0[l]);
				}
				for (int j=0; j<NWAY; ++j) {
					y1[j]  = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				for (int j=0; j<NWAY; ++j) {
					ro[j] += y1[j] * vall(Ql0[l+1]);
				}
				al+=4;	l+=2;
			}
			if (l==llim) {
				for (int j=0; j<NWAY; ++j) {
					y0[j]  = vall(al[1])*cost[j]*y1[j] + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {
					re[j] += y0[j] * vall(Ql0[l]);
				}
			}
			// combine even/odd into north/south
			for (int j=0; j<NWAY; ++j) {
				rnd s = re[j] - ro[j];		re[j] = re[j] + ro[j];
				ro[j] = s;
			}
		#ifndef SHTNS4MAGIC
			for (int j=0; j<NWAY; ++j) {
				S2D_STORE(BrF, j+k, re[j], ro[j])
			}
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
				S2D_STORE_4MAGIC((double*)BrF, j+k, re[j], ro[j]);
			}
		#endif
			k+=NWAY;
		} while (k < nk);
	}
  #ifndef SHT_AXISYM
	else
	{		// im > 0
		BrF += im*(shtns->nlat_padded >>1);
		m = im*MRES;
		l = (im*(2*(LMAX+1)-(m+MRES)))>>1;		//l = LiM(shtns, 0,im);
		#ifndef SHTNS_ISHIOKA
		alm = shtns->alm + 2*(l+m);		// shtns->alm + im*(2*(LMAX+1) -m+MRES);
		#else
		alm = shtns->clm + (l+m);		// shtns->clm + im*(2*(LMAX+1) -m+MRES)/2;
		#endif

  #ifndef SHT_GRAD
  #else
  #endif

	#ifndef SHTNS_ISHIOKA
		cplx* Ql = &Qlm[l];	// virtual pointer for l=0 and im
	#else
		// pre-processing for recurrence relation of Ishioka
		const double* restrict xlm = shtns->xlm + 3*im*(2*(LMAX+4) -m+MRES)/4;
		v2d* Ql = (v2d*) &Qlm[l];	// virtual pointer for l=0 and im
		SH_to_ishioka(xlm, Ql+m, llim-m, QQl+m);
	#endif

		// polar optimization
		k = shtns->tm[im];		// start index in theta (=0 if no polar optimization)
		#if _GCC_VEC_
		k = ((unsigned) k) / VSIZE2;	// in vector size units.
		#else
		k = (k>>1)*2;		// k must be even.
		#endif
		if (it0 < k) {
			const long ofsm = (NPHI-2*im)*(shtns->nlat_padded >>1);
		#ifndef SHTNS4MAGIC
			#if _GCC_VEC_
			const long ofs1 = NLAT_2 - k*(VSIZE2/2);
			#else
			const long ofs1 = NLAT_2 - k/2;
			#endif
			zero_poles4_vect(BrF+it0*(VSIZE2/2), ofsm, ofs1, k-it0);
		#else
			zero_poles2_vect(BrF+it0*VSIZE2, ofsm, 2*(k-it0));
		#endif
		} else k=it0;

		do {
			rnd cost[NWAY], y0[NWAY], y1[NWAY];
			rnd rer[NWAY], rei[NWAY], ror[NWAY], roi[NWAY];
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(st, k+j);
				y0[j] = vall(1.0);
			}
			l=m;
	#ifndef HI_LLIM
			const long int ny = 0;
			while(1) {		// sin(theta)^m
				if (l&1) for (int j=0; j<NWAY; ++j) y0[j] *= cost[j];
				l >>= 1;
				if (l==0) break;
				for (int j=0; j<NWAY; ++j) cost[j] *= cost[j];
			}
	#else
			long int ny = 0;
			for (long int nsint = 0;;) {		// sin(theta)^m		(use rescaling to avoid underflow)
				if (l&1) {
					for (int j=NWAY-1; j>=0; --j) y0[j] *= cost[j];
					ny += nsint;
					if (vlo(y0[NWAY-1]) < (SHT_ACCURACY+1.0/SHT_SCALE_FACTOR)) {
						ny--;
						for (int j=NWAY-1; j>=0; --j) y0[j] *= vall(SHT_SCALE_FACTOR);
					}
				}
				l >>= 1;
				if (l==0) break;
				for (int j=NWAY-1; j>=0; --j) cost[j] *= cost[j];
				nsint += nsint;
				if (vlo(cost[NWAY-1]) < 1.0/SHT_SCALE_FACTOR) {
					nsint--;
					for (int j=NWAY-1; j>=0; --j) cost[j] *= vall(SHT_SCALE_FACTOR);
				}
			}
	#endif
			al = alm;
			for (int j=0; j<NWAY; ++j) {
				cost[j] = vread(ct, j+k);
				ror[j] = vall(0.0);		roi[j] = vall(0.0);
				rer[j] = vall(0.0);		rei[j] = vall(0.0);
				#ifndef SHTNS_ISHIOKA
				y0[j] *= vall(al[0]);
				#else
				cost[j] *= cost[j];		// cos(theta)^2
				#endif
			}
			for (int j=0; j<NWAY; ++j) {
				#ifndef SHTNS_ISHIOKA
				y1[j]  = (vall(al[1])*y0[j]) *cost[j];		//	y1[j] = vall(al[1])*cost[j]*y0[j];
				#else
				y1[j] = (vall(al[1])*cost[j] + vall(al[0]))*y0[j];
				#endif
			}
			l=m;		al+=2;
	#ifdef HI_LLIM
		  if (ny < 0) {		// ylm treated as zero and ignored if ny < 0
			const rnd scale = vall(1.0/SHT_SCALE_FACTOR);
			while (l<llim) {
				#ifndef SHTNS_ISHIOKA
				for (int j=0; j<NWAY; ++j)	y0[j] = (vall(al[1])*cost[j])*y1[j] + vall(al[0])*y0[j];
				for (int j=0; j<NWAY; ++j)	y1[j] = (vall(al[3])*cost[j])*y0[j] + vall(al[2])*y1[j];
				al+=4;
				#else
				rnd a[NWAY];
				for (int j=0; j<NWAY; ++j)	a[j] = vall(al[1])*cost[j] + vall(al[0]);
				al+=2;
				for (int j=0; j<NWAY; ++j) {
					a[j] = a[j]*y1[j] + y0[j];
					y0[j] = y1[j];		y1[j] = a[j];
				}
				#endif
				l+=2;
				if (fabs(vlo(y0[NWAY-1])) > SHT_ACCURACY*SHT_SCALE_FACTOR + 1.0) {		// rescale when value is significant
					for (int j=0; j<NWAY; ++j) {
						y0[j] *= scale;		y1[j] *= scale;
					}
					if (++ny == 0) break;
				}
			}
		  }
	#endif
		  if LIKELY(ny == 0) {
		#ifndef SHTNS_ISHIOKA
			while (l<llim) {	// compute even and odd parts
				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
				for (int j=0; j<NWAY; ++j) {
					y0[j] = vall(al[1])*(cost[j]*y1[j]) + vall(al[0])*y0[j];
				}
				for (int j=0; j<NWAY; ++j) {	ror[j] += y1[j]  * qr(l+1);		roi[j] += y1[j] * qi(l+1);	}
				for (int j=0; j<NWAY; ++j) {
					y1[j] = vall(al[3])*(cost[j]*y0[j]) + vall(al[2])*y1[j];
				}
				l+=2;	al+=4;
			}
			if (l==llim) {
				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
			}
			// combine even/odd into north/south
			for (int j=0; j<NWAY; ++j) {
				rnd sr = rer[j] - ror[j];	rer[j] = rer[j] + ror[j];
			  	rnd si = rei[j] - roi[j];	rei[j] = rei[j] + roi[j];
				ror[j] = sr;		roi[j] = si;
			}
		#else
			while (l<llim) {	// compute even and odd parts
				for (int j=0; j<NWAY; ++j) {	rer[j] += y0[j]  * qr(l);		rei[j] += y0[j] * qi(l);	}
				for (int j=0; j<NWAY; ++j) {	ror[j] += y0[j]  * qr(l+1);		roi[j] += y0[j] * qi(l+1);	}
				for (int j=0; j<NWAY; ++j) {
					rnd tmp = (vall(al[1])*cost[j] + vall(al[0]))*y1[j] + y0[j];
					y0[j] = y1[j];
					y1[j] = tmp;
				}
				l+=2;	al+=2;
			}
			for (int j=0; j<NWAY; ++j) cost[j] = vread(ct, k+j);		// read ahead to correct the odd part below
			if LIKELY(l==llim) {
				for (int j=0; j<NWAY; ++j)	rer[j] += y0[j] * qr(l);
				for (int j=0; j<NWAY; ++j)	rei[j] += y0[j] * qi(l);
			}
			// correct the odd part:
		//	for (int j=0; j<NWAY; ++j) {  ror[j] *= cost[j];	roi[j] *= cost[j]; }

			// combine even/odd into north/south, and correct the odd part for free with FMA
			for (int j=0; j<NWAY; ++j) {
				rnd sr = rer[j] - ror[j]*cost[j];	rer[j] = rer[j] + ror[j]*cost[j];
			  	rnd si = rei[j] - roi[j]*cost[j];	rei[j] = rei[j] + roi[j]*cost[j];
				ror[j] = sr;		roi[j] = si;
			}
		#endif
		  }
		#ifndef SHTNS4MAGIC
		  #ifdef _GCC_VEC_
			const long ofs = (NPHI-2*im)*shtns->nlat_padded;
			for (int j=0; j<NWAY; ++j) cstore_north_south((double*) BrF, ((double*) (BrF)) +ofs, k+j, NLAT, rer[j], ror[j], rei[j], roi[j]);
		  #else
		  	// NWAY is even when _GCC_VEC_ is not defined
			for (int j=0; j<NWAY/2; ++j) {	S2D_CSTOREX(BrF, k/2+j, 2*j, rer, ror, rei, roi)  }
		  #endif
		#else
			for (int j=0; j<NWAY; ++j) {
				if ((k+j)>=nk) break;
				S2D_CSTORE_4MAGIC((double*) BrF, (double*) (BrF + (NPHI-2*im)*(shtns->nlat_padded>>1)), k+j, rer[j], ror[j], rei[j], roi[j]);
			}
		#endif
			k+=NWAY;
		} while (k < nk);
	}
  #endif
}

	#undef qr
	#undef qi

	#undef BASE
