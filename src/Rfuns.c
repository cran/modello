/*
 * This file is part of Modello.
 * 
 * Modello is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 */
 
#include <R.h>
#include <Rmath.h>

/*
 * _DOC_
 * This modules contains C wrapper functios to the some of the R core functions which are used within Modello.
 * I it is required to use further R core functions, the corresponding C wrapper must be defined in this file.
 *  
 */

//* Errors and Warnings

void r__error_ (char *msg) {
	/*
	 * _DOC_
	 * To send error messages to the R consle. 
	 */
	//_MAIN_
	error(msg);
}

void r__warn_ (char *msg) {
	/*
	 * _DOC_
	 * To send warning messages to the R console
	 */
	//_MAIN_ 
	warning(msg);
}

//* SPECIAL MATH FUNCTION

//** Beta function
double r__beta (double *a, double *b) {
	/*
	 * _DOr__
	 * Wrapper to the R function beta, calulating the beta fucntion of two real numbers.
	 * _VARIABLES_
	 * in/out
	 * a, b: two real numbers
	 */
	//MAIN
	return(beta(a[0], b[0]));
}
double r__lbeta (double *a, double *b) {
	/*
	 * DOC
	 * Wrapper to the R function lbeta, calulating the ln beta fucntion of two real numbers.
	 * VARIABLES
	 * in/out
	 * a, b: two real numbers
	 */
	//MAIN
	return(lbeta(a[0], b[0]));
}

//** Gamma function
double r__gamma (double *a) {
	/*
	 * DOC
	 * Wrapper to rhe R function gamma, calculationg the gamma function of a real number.
	 * VARIABLES
	 * in/out
	 * a: a real number
	 */
	//_MAIN_
	return(gamma(a[0]));
}

double r__lgamma (double *a) {
	/*
	 * DOC
	 * Wrapper to rhe R function lgamma, calculating the ln gamma function of a real number.
	 * VARIABLES
	 * in/out
	 * a: a real number
	 */
	//_MAIN_
	return(lgamma(a[0]));
}

//* PROBABILITY DISTRIBUTIONS

//** Uniform distribution
double r__runif (double *a, double *b) {
	/*
	 * DOC
	 * Wrapper to the R function runif. 
	 * Returns a realization of a random varialbe with uniform distribution in [a, b].
	 */ 
	//VARIABLES
	//Dummy
	//a, b: lower and upper bound of the uniform distribution. 
	//auxiliaryiliary
	double x;	//stores the generated value
	//MAIN
	GetRNGstate();
    x = runif(a[0], b[0]);
    PutRNGstate();
	return(x);
}
double r__dunif (double *x, double *a, double *b, int *ln) {
	/*
	 * DOC
	 * Wrapper to the R function dunif.
	 * Returns the uniform probability density function of x in [a, b].
	 */
	//VARIABLES
	//Dummy
	//x: value of the variable.
	//a, b: lower and upper bounds of the distribution.
	//ln: > 0 => returns ln(dunif)
	//MAIN	
	return(dunif(x[0], a[0], b[0], ln[0]));
}
double r__punif (double *x, double *a, double *b, int *lwt, int *ln) {
	/*
	 * DOC
	 * Wrapper to the R function punif.
	 * Returns the cumulative density distribution of the quantile x in [a, b].
	 */
	//VARIABLES
	//Dummy
	//x: quantile.
	//a, b: lower and upper bounds of the uniform distribution.
	//lwt: > 0 => returns P(X <= x), < 0 => returns P(X > x).
	//ln: > 0 => returns ln(punif)
	//MAIN
	return(punif(x[0], a[0], b[0], lwt[0], ln[0]));
}
double r__qunif (double *x, double *a, double *b, int *lwt, int *ln) {
	/*
	 * DOC
	 * Wrapper to the R function qunif.
	 * Qunatile function of the uniform dostribution.
	 */	
	//VARIABLES
	//x: probability.
	//a, b: lower and upper bounds of the uniform distribution.
	//lwt: > 0 => considers x = P(X <= x), < 0 => considers x = P(X > x).
	//ln: > 0 => considers x = ln(punif)
	//MAIN 
	return(qunif(x[0], a[0], b[0], lwt[0], ln[0]));
}
 	
//** BETA DISTRIBUTION
double r__rbeta (double *shp1, double *shp2) {
	/*
	 * DOC
	 * Wrapper of the R function rbeta.
	 * Generates randomly beta distributed variables.
	 */
	//VARIABLES
	//Dummy
	//shp1, shp2: shape parameters of the beta distribution 
	//auxiliaryiliary
	double x;
	//MAIN
    GetRNGstate();
    x = rbeta(shp1[0], shp2[0]);
    PutRNGstate();
    return(x);
}
double r__dbeta (double *x, double *shp1, double *shp2, int *ln) {
	/*
	 * DOC 
	 * Wrapper function to the R function dbeta, which calculates the probability density distrbution
	 * of beta distributed random variables.
	 */ 
	//VARIABLES
	//Dummy
	//x: value.
	//shp1, shp2: shape parameters of the beta distribution. 
	//ln: > 0 => returns ln(dbeta) 
	//MAIN
	return(dbeta(x[0], shp1[0], shp2[0], ln[0]));
}
double r__pbeta (double *x, double *shp1, double *shp2, int *lwt, int *ln) {
	/*
	 * DOC
	 * Wrapper function to the R function pbeta, which the calculates the cumulative distrbution 
	 * of beta distributed random variables.
	 */
	//VARIABLES
	//Dummy
	//x: quantile.
	//shp1, shp2: shape parameters of the beta distribution 
	//lwt: > 0 => returns P(X <= x), < 0 => returns P(X > x)
	//ln: > 0 => returns ln(pbeta)
	//MAIN
	return(pbeta(x[0], shp1[0], shp2[0], lwt[0], ln[0]));
}
double r__qbeta (double *x, double *shp1, double *shp2, int *lwt, int *ln) {
	//DOC
	//Wrapper function to the R function pbeta, which the calculates the cumulative distrbution 
	//of beta distributed random variables.
	//VARIABLES
	//Dummy
	//x: probability.
	//shp1, shp2: shape parameters of the beta distribution 
	//lwt: > 0 => considers x = P(X <= x), < 0 => considers x = P(X >= x)
	//ln: > 0 => consideres x = ln(pbeta)
	//MAIN
	return(qbeta(x[0], shp1[0], shp2[0], lwt[0], ln[0]));
}

//** GAMMA DISTRIBUTION
double r__rgamma (double *shp, double *rt) {
	//DOC
	//Wrapper function to the R function rgamma, which generate 1 realizattion of a gamma distributed random varialbe.
	//VARIABLES
	//Dummy
	//shp, rt: shape and rate parameter fo the gamma density probability distribution 
	//auxiliaryiliary
	double x;	//realization of the gamma distrbuted variable.
	//MAIN
	GetRNGstate();
    x = rgamma(shp[0], 1/rt[0]);
    PutRNGstate();
	return(x);
}
double r__dgamma (double *x, double *shp, double *rt, int *ln) {
	//DOC
	//Wrapper to the R function dgamma, which represents the gamma probability density distribution.
	//VARIABLES
	//Dummy
	//x: value.
	//shp, rt: shape and rate parameters.
	//ln: > 0 => returns ln(dgamma) 
	//MAIN
	return(dgamma(x[0], shp[0], 1/rt[0], ln[0]));
}
double r__pgamma (double *x, double *shp, double *rt, int* lwt, int *ln) {
	//DOC
	//Wrapper to the R function pgamma, which represents the gamma cumulative probability function.
	//VARIABLES
	//Dummy
	//x: quantile
	//shp, rt: shape and rate parameters.
	//lwt: > 0 => returns P(X <= x), < 0 => returns P(X > x)
	//ln: > 0 => returns ln(pgamma) 
	//MAIN
	return(pgamma(x[0], shp[0], 1/rt[0], lwt[0], ln[0]));
}
double r__qgamma (double *x, double *shp, double *rt, int* lwt, int *ln) {
	//DOC
	//Wrapper to the R function qgamma, which represnets the quantile function of the gamma distribution.
	//VARIABLES
	//x: proibability.
	//shp, rt: shape and rate parameters.
	//lwt: > 0 => considers x = P(X <= x), < 0 => considers x = P(X >= x)
	//ln: > 0 => consideres x = ln(pbeta)
	//MAIN
	return(qgamma(x[0], shp[0], 1/rt[0], lwt[0], ln[0]));
}

//** NORMAL DISTRIBUTION
double r__rnorm (double *mu, double *sigma) {
	//DOC
	//Wrapper to the R function rnorm, which genrates a single realization from a random variable 
	//normally distributed.
	//VARIABLES
	//mu, sigma: mean and standard deviation.
	//auxiliaryiliary
	double x;	//used to store teh genrated value.
	//MAIN
	GetRNGstate();
	x = rnorm(mu[0], sigma[0]);
	PutRNGstate();
	return(x);
}
double r__dnorm (double *x, double *mu, double *sigma, int *ln) {
	//DOC
	//Wrapper to the R function dnorm, whioch represents the normal probability density distribution.
	//VARIABLES
	//x: value.
	//mu, sigma: mean and standard deviation.
	//ln: > 0 => returns ln(dnorm)
	//MAIN
	return(dnorm(x[0], mu[0], sigma[0], ln[0]));
}
double r__pnorm (double *x, double *mu, double *sigma, int* lwt, int *ln) {
	//DOC
	//Wrapper to the R function pnorm, which represents the cumulative function of the normal distribution.
	//VARIABLES
	//x: quantile.
	//mu, sigma: mean and standard deviation.
	//lwt: > 0 => returns P(X <= x), < 0 => returns P(X > x)
	//ln: > 0 => returns ln(pnorm) 
	//MAIN
	return(pnorm(x[0], mu[0], sigma[0], lwt[0], ln[0]));
}
double r__qnorm (double *x, double *mu, double *sigma, int* lwt, int *ln) {
	//DOC
	//Wrapper to the R function qnorm, which represnets the quantile function of the normal distribution.
	//VARIABLES
	//x: probability.
	//mu, sigma: mean and standard deviation.
	//lwt: > 0 => considers x = P(X <= x), < 0 => considers x = P(X >= x)
	//ln: > 0 => consideres x = ln(pnorm)
	//MAIN
	return(qnorm(x[0], mu[0], sigma[0], lwt[0], ln[0]));
}

//** EXPONENTIAL DISTRIBUTION
double r__rexp (double *rt) {
	//DOC
	//Wrapper to the R function rexp, which generates a realisation of a random variable with exponential 
	//distribtuion.
	//VARIABLES
	//Dummy
	//rt: rate parameter.
	//auxiliaryiliry
	double x;	//used to store the generated value
	//MAIN
	GetRNGstate();
	x = rexp(1/rt[0]);
	PutRNGstate();
	return(x);
}
double r__dexp (double *x, double *rt, int *ln) {
	//DOC
	//Wrapper to the R function dexp, which represnets the exponential probability density distrbution.
	//VARIABLES
	//Dummy
	//x: value.
	//rt: rate parameter.
	//ln: > 0 => returns ln(dexp)
	//MAIN 
	return(dexp(x[0], 1/rt[0], ln[0]));
}
double r__pexp (double *x, double *rt, int *lwt, int *ln) {
	/* 
	 * DOC
	 * Wrapper top the R function pexp, whcih represents the cumulative function for the exponential distribution.
	 */
	//VARIABLES
	//Dummy
	//x: quantile.
	//rt: rate parameter.
	//lwt: > 0 => returns P(X <= x), < 0 => returns P(X > x)
	//ln: > 0 => returns ln(pnorm) 
	//MAIN
	return(pexp(x[0], 1/rt[0], lwt[0], ln[0]));
}
double r__qexp (double *x, double *rt, int *lwt, int *ln) {
	/*
	 * DOC
	 * Wrapper to the R function qexp, which represnets the quantile function for the exponential distribution.
	 * VARIABLES
	 * Dummy
	 * x: probability
	 * rt: rate parameter.
	 * lwt: > 0 => considers x = P(X <= x), < 0 => considers x = P(X >= x)
	 * ln: > 0 => consideres x = ln(pexp)
	 */ 
	 //MAIN
	return(qexp(x[0], 1/rt[0], lwt[0], ln[0]));
}
