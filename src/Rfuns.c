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
	error(msg);
}

void r__warn_ (char *msg) {
	/*
	 * _DOC_
	 * To send warning messages to the R console
	 */
	warning(msg);
}

