/*------------------------------------------------------------------------------
© A J S Hamilton 2003
------------------------------------------------------------------------------*/
#ifndef MANGLEFN_H
#define MANGLEFN_H

#include "defines.h"
#include "format.h"
#include "harmonics.h"
#include "logical.h"
#include "polygon.h"
#include "vertices.h"
#include "polysort.h"

void	advise_fmt(format *);

void	azel_(double *, double *, double *, double *, double *, double *, double *);
void	azell_(double *, double *, double *, double *, double *, double *, double *, double *, double *);

void	braktop(double, int *, double [], int, int);
void	brakbot(double, int *, double [], int, int);
void	braktpa(double, int *, double [], int, int);
void	brakbta(double, int *, double [], int, int);
void	braktop_(double *, int *, double [], int *, int *);
void	brakbot_(double *, int *, double [], int *, int *);
void	braktpa_(double *, int *, double [], int *, int *);
void	brakbta_(double *, int *, double [], int *, int *);

void	cmminf(polygon *, int *, double *);

void	vert_to_poly(vertices *, polygon *);
void	edge_to_poly(vertices *, int, int *, polygon *);
void    rect_to_poly(double [4], polygon *);

#ifdef	GCC
void	rps_to_vert(int nv, vec [nv], vertices *);
#else
void	rps_to_vert(int nv, vec [/*nv*/], vertices *);
#endif
void	rp_to_azel(vec, azel *);
void	azel_to_rp(azel *, vec);
void	azel_to_gc(azel *, azel *, vec, double *);
void	rp_to_gc(vec, vec, vec, double *);
void	edge_to_rpcm(azel *, azel *, azel *, vec, double *);
void	rp_to_rpcm(vec, vec, vec, vec, double *);
void	circ_to_rpcm(double [3], vec, double *);
void	rpcm_to_circ(vec, double *, double [3]);
void	az_to_rpcm(double, int, vec, double *);
void	el_to_rpcm(double, int, vec, double *);
double	thij(vec, vec);
double	cmij(vec, vec);
int	poly_to_rect(polygon *, double *, double *, double *, double *);
int	antivert(vertices *, polygon *);

void	copy_format(format *, format *);

void	copy_poly(polygon *, polygon *);
void	copy_polyn(polygon *, int, polygon *);
void	poly_poly(polygon *, polygon *, polygon *);
void	poly_polyn(polygon *, polygon *, int, int, polygon *);
#ifdef	GCC
void	group_poly(polygon *poly, int [poly->np], int, polygon *);
#else
void	group_poly(polygon *poly, int [/*poly->np*/], int, polygon *);
#endif

void    assign_parameters();
void    pix2ang(int, unsigned long, double *, double *);
void    ang2pix(int, double, double, unsigned long *);
void    pix2ang_radec(int, unsigned long, double *, double *);
void    ang2pix_radec(int, double, double, unsigned long *);
void    csurvey2eq(double, double, double *, double *);
void    eq2csurvey(double, double, double *, double *);
void    superpix(int, unsigned long, int, unsigned long *);
void    subpix(int, unsigned long, unsigned long *, unsigned long *, unsigned long *, unsigned long *);
void    pix_bound(int, unsigned long, double *, double *, double *, double *);
double  pix_area(int, unsigned long);
void    pix2xyz(int, unsigned long, double *, double *, double *);
void    area_index(int, double, double, double, double, unsigned long *, unsigned long *, unsigned long *, unsigned long *);
void    area_index_stripe(int, int, unsigned long *, unsigned long *, unsigned long *, unsigned long *);

double	drandom(void);

#ifdef	GCC
int	cmlim_polys(int npoly, polygon *[npoly], double, vec);
int	drangle_polys(int npoly, polygon *[npoly], double, vec, int nth, double [nth], double [nth]);
#else
int	cmlim_polys(int npoly, polygon *[/*npoly*/], double, vec);
int	drangle_polys(int npoly, polygon *[/*npoly*/], double, vec, int nth, double [/*nth*/], double [/*nth*/]);
#endif

void	cmlimpolys_(double *, vec);
#ifdef	GCC
void	dranglepolys_(double *, vec, int *nth, double [*nth], double [*nth]);
#else
void	dranglepolys_(double *, vec, int *nth, double [/**nth*/], double [/**nth*/]);
#endif

#ifdef	GCC
void	dump_poly(int npoly, polygon *[npoly]);
#else
void	dump_poly(int, polygon *[/*npoly*/]);
#endif

void	fframe_(int *, double *, double *, int *, double *, double *);

void	findtop(double [], int, int [], int);
void	findbot(double [], int, int [], int);
void	findtpa(double [], int, int [], int);
void	findbta(double [], int, int [], int);
void	finitop(int [], int, int [], int);
void	finibot(int [], int, int [], int);
void	finitpa(int [], int, int [], int);
void	finibta(int [], int, int [], int);

void	findtop_(double [], int *, int [], int *);
void	findbot_(double [], int *, int [], int *);
void	findtpa_(double [], int *, int [], int *);
void	findbta_(double [], int *, int [], int *);
void	finitop_(int [], int *, int [], int *);
void	finibot_(int [], int *, int [], int *);
void	finitpa_(int [], int *, int [], int *);
void	finibta_(int [], int *, int [], int *);

polygon *get_pixel(int,char);
int     get_child_pixels(int, int [], char);
int     get_parent_pixels(int, int [], char);
int     get_res(int,char);

void    healpix_ang2pix_nest(int, double, double, int *);
polygon *get_healpix_poly(int, int);
int     get_nside(int);
void    healpix_verts(int, int, vec, double []);
void    pix2vecnest_(int *, int *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *, double *);
double  cmrpirpj(vec, vec);

int	garea(polygon *, double *, int, double *);
int	gcmlim(polygon *, double *, vec, double *, double *);
int	gphbv(polygon *, int, int, double *, double [2], double [2]);
int	gphi(polygon *, double *, vec, double, double *);
int	gptin(polygon *, vec);
#ifdef	GCC
int	gspher(polygon *, int lmax, double *, double *, double [2], double [2], harmonic [NW]);
int	gsphera(double, double, double, double, int lmax, double *, double [2], double [2], harmonic [NW]);
int	gsphr(polygon *, int lmax, double *, harmonic [NW]);
int	gsphra(double, double, double, double, int lmax, harmonic [NW]);
#else
int	gspher(polygon *, int lmax, double *, double *, double [2], double [2], harmonic [/*NW*/]);
int	gsphera(double, double, double, double, int lmax, double *, double [2], double [2], harmonic [/*NW*/]);
int	gsphr(polygon *, int lmax, double *, harmonic [/*NW*/]);
int	gsphra(double, double, double, double, int lmax, harmonic [/*NW*/]);
#endif
int	gverts(polygon *, int, double *, int, int, int *, vec **, double **, int **, int **, int *, int *, int **);
#ifdef	GCC
int	gvert(polygon *poly, int, double *, int nvmax, int, int nve, int *, vec [nvmax * nve], double [nvmax], int [nvmax], int [poly->np], int *, int *, int [nvmax]);
#else
int	gvert(polygon *poly, int, double *, int nvmax, int, int nve, int *, vec [/*nvmax * nve*/], double [/*nvmax*/], int [/*nvmax*/], int [/*poly->np*/], int *, int *, int [/*nvmax*/]);
#endif
int	gvlims(polygon *, int, double *, vec, int *, vec **, vec **, double **, double **, double **, double **, int **, int **, int *, int *, int **);
#ifdef	GCC
int	gvlim(polygon *poly, int, double *, vec, int nvmax, int *, vec [nvmax], vec [nvmax], double [nvmax], double [nvmax], double [poly->np], double [poly->np], int [nvmax], int [poly->np], int *, int *, int [nvmax]);
#else
int	gvlim(polygon *poly, int, double *, vec, int nvmax, int *, vec [/*nvmax*/], vec [/*nvmax*/], double [/*nvmax*/], double [/*nvmax*/], double [/*poly->np*/], double [/*poly->np*/], int [/*nvmax*/], int [/*poly->np*/], int *, int *, int [/*nvmax*/]);
#endif
int	gvphi(polygon *, vec, double, vec, double *, double *, vec);

void	garea_(double *, vec [], double [], int *, double *, int *, double *, int *, logical *);
void	gaxisi_(vec, vec, vec);
void	gcmlim_(vec [], double [], int *, vec, double *, double *, double *, double *, int *);
void	gphbv_(double [2], double [2], vec [], double [], int *, int *, int *, int *, double *, double *, int *);
void	gphi_(double *, vec [], double [], int *, vec, double *, double *, double *, int *);
logical	gptin_(vec [], double [], int *, vec);
void	gspher_(double *, double [2], double [2], harmonic [], int *, int *, int *, vec [], double [], int *, int *, int *, int *, double *, double *, int *, double *, logical *);
void	gsphera_(double *, double [2], double [2], harmonic [], int *, int *, int *, int *, double *, double *, double *, double *, double *, double *);
void	gvert_(vec [], double [], int [], int [], int [], int *, int *, int *, int *, int *, int *, vec [], double [], int *, int *, double *, double *, int *, double *, int *, logical *);

void	gvlim_(vec [], vec [], double [], double [], double [], double [], int [], int [], int [], int *, int *, int *, int *, vec [], double [], int *, double [], int *, double *, double *, int *, double *, int *, logical *);
void	gvphi_(double *, vec, vec [], double [], int *, vec, double *, vec, double *, double *, int *);

#ifdef	GCC
int	harmonize_polys(int npoly, polygon *[npoly], double, int lmax, harmonic w[NW]);
#else
int	harmonize_polys(int npoly, polygon *poly[/*npoly*/], double, int lmax, harmonic w[/*NW*/]);
#endif

void	harmonizepolys_(double *, int *, harmonic []);

void	ikrand_(int *, double *);
void	ikrandp_(double *, double *);
void	ikrandm_(double *, double *);

void	msg(char *, ...);

polygon	*new_poly(int);
void	free_poly(polygon *);
int	room_poly(polygon **, int, int, int);
void	memmsg(void);

vertices	*new_vert(int);
void	free_vert(vertices *);

void	parse_args(int, char *[]);

int	parse_fopt(void);

#ifdef	GCC
int	partition_poly(polygon **, int npolys, polygon *[npolys], double, int, int, int, int, int *);
int	partition_gpoly(polygon *, int npolys, polygon *[npolys], double, int, int, int, int *);
int	part_poly(polygon *, int npolys, polygon *[npolys], double, int, int, int, int *, int *);
int     pixel_list(int npoly, polygon *[npoly], int max_pixel, int [max_pixel], int [max_pixel]);
#else
int	partition_poly(polygon **, int npolys, polygon *[/*npolys*/], double, int, int, int, int, int *);
int	partition_gpoly(polygon *, int npolys, polygon *[/*npolys*/], double, int, int, int, int *);
int	part_poly(polygon *, int npolys, polygon *[/*npolys*/], double, int, int, int, int *, int *);
int     pixel_list(int npoly, polygon *[/*npoly*/], int max_pixel, int [/*max_pixel*/], int [/*max_pixel*/]);
#endif
int     pixel_start(int, char);

double	places(double, int);
int     poly_cmp(polygon **, polygon **);

#ifdef	GCC
int	poly_id(int npoly, polygon *[npoly], double, double, int **, double **);
void	poly_sort(int npoly, polygon *[npoly], char);
#else
int	poly_id(int npoly, polygon *[/*npoly*/], double, double, int **, double**);
void	poly_sort(int npoly, polygon *[/*npoly*/], char);
#endif


int	prune_poly(polygon *, double);
int	trim_poly(polygon *);
int	touch_poly(polygon *);

int	rdangle(char *, char **, char, double *);

#ifdef	GCC
int	rdmask(char *, format *, int npolys, polygon *[npolys]);
#else
int	rdmask(char *, format *, int npolys, polygon *[/*npolys*/]);
#endif

void	rdmask_(void);

int	rdspher(char *, int *, harmonic **);

void	scale(double *, char, char);
void	scale_azel(azel *, char, char);
void	scale_vert(vertices *, char, char);

#ifdef	GCC
int	search(int n, double [n], double);
#else
int	search(int n, double [/*n*/], double);
#endif

#ifdef	GCC
int	snap_polys(format *fmt, int npoly, polygon *poly[npoly], int, double, double, double, double, double, int, char *);
#else
int	snap_polys(format *fmt, int npoly, polygon *poly[/*npoly*/], int, double, double, double, double, double, int, char *);
#endif
int	snap_poly(polygon *, polygon *, double, double);
int	snap_polyth(polygon *, polygon *, double, double, double);

int	split_poly(polygon **, polygon *, polygon **, double, char);
#ifdef	GCC
int	fragment_poly(polygon **, polygon *, int, int npolys, polygon *[npolys], double, char);
#else
int	fragment_poly(polygon **, polygon *, int, int npolys, polygon *[/*npolys*/], double, char);
#endif

int	strcmpl(const char *, const char *);
int	strncmpl(const char *, const char *, size_t);

int	strdict(char *, char *[]);
int	strdictl(char *, char *[]);

#ifdef	GCC
int	vmid(polygon *, double, int nv, int nve, vec [nv * nve], int [nv], int [nv], int *, vec **);
int	vmidc(polygon *, int nv, int nve, vec [nv * nve], int [nv], int [nv], int *, vec **);
#else
int	vmid(polygon *, double, int nv, int nve, vec [/*nv * nve*/], int [/*nv*/], int [/*nv*/], int *, vec **);
int	vmidc(polygon *, int nv, int nve, vec [/*nv * nve*/], int [/*nv*/], int [/*nv*/], int *, vec **);
#endif

double	weight_fn(double, double, char *);
double	rdweight(char *);

double	twoqz_(double *, double *, int *);
double	twodf100k_(double *, double *);
double	twodf230k_(double *, double *);

int     which_pixel(double, double, int, char);

#ifdef	GCC
void	wrangle(double, char, int, size_t str_len, char [str_len]);
#else
void	wrangle(double, char, int, size_t str_len, char [/*str_len*/]);
#endif

#ifdef	GCC
double	wrho(double, double, int lmax, int, harmonic w[NW], double, double);
#else
double	wrho(double, double, int lmax, int, harmonic w[/*NW*/], double, double);
#endif

double	wrho_(double *, double *, harmonic *, int *, int *, int *, int *, double *, double *);

#ifdef	GCC
int	wrmask(char *, format *, int npolys, polygon *[npolys]);
int	wr_circ(char *, format *, int npolys, polygon *[npolys], int);
int	wr_edge(char *, format *, int npolys, polygon *[npolys], int);
int	wr_rect(char *, format *, int npolys, polygon *[npolys], int);
int	wr_poly(char *, format *, int npolys, polygon *[npolys], int);
int	wr_Reg(char *, format *, int npolys, polygon *[npolys], int);
int	wr_area(char *, format *, int npolys, polygon *[npolys], int);
int	wr_id(char *, int npolys, polygon *[npolys], int);
int	wr_midpoint(char *, format *, int npolys, polygon *[npolys], int);
int	wr_weight(char *, format *, int npolys, polygon *[npolys], int);
int     wr_healpix_weight(char *, format *, int numweight, double [numweight]);
int	wr_list(char *, format *, int npolys, polygon *[npolys], int);
int	discard_poly(int npolys, polygon *[npolys]);
#else
int	wrmask(char *, format *, int npolys, polygon *[/*npolys*/]);
int	wr_circ(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_edge(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_rect(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_poly(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_Reg(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_area(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_id(char *, int npolys, polygon *[/*npolys*/], int);
int	wr_midpoint(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	wr_weight(char *, format *, int npolys, polygon *[/*npolys*/], int);
int     wr_healpix_weight(char *, format *, int numweight, double [/*numweight*/]);
int	wr_list(char *, format *, int npolys, polygon *[/*npolys*/], int);
int	discard_poly(int npolys, polygon *[/*npolys*/]);
#endif

int	wrrrcoeffs(char *, double, double [2], double [2]);

#ifdef	GCC
int	wrspher(char *, int lmax, harmonic [NW]);
#else
int	wrspher(char *, int lmax, harmonic [/*NW*/]);
#endif

#endif	/* MANGLEFN_H */
