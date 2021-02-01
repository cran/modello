#  This file is part of Modello.
#
#  Modello is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#  
#  Modello is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.

RBIN = "${R_HOME}/bin/R"

.PHONY: clean

all: modello.so clean

modello.so: Rinterfaces.c \
	env.o \
	types.o \
	registers.o \
	utils.o \
	errwarn.o \
	Rfuns.o\
	math.o \
	operators.o \
	node_operators.o \
	numbers_utils.o \
	nodes_utils.o\
	nodes.o \
	numbers.o \
	numbers_modifiers.o \
	numbers_math.o \
	optim_utils.o \
	sgd.o \
	interfaces.o 
	${RBIN} CMD SHLIB -o modello.so \
		Rinterfaces.c \
		env.o \
		types.o \
		registers.o \
		utils.o \
		errwarn.o \
		Rfuns.o \
		math.o \
		operators.o \
		node_operators.o \
		numbers_utils.o \
		nodes_utils.o \
		nodes.o \
		numbers.o \
		numbers_modifiers.o \
		numbers_math.o\
		optim_utils.o \
		sgd.o \
		interfaces.o

#R functions
Rfuns.o: Rfuns.c
	${RBIN} CMD COMPILE Rfuns.c
#Environmental parameters
env.o: env.f95		
	${RBIN} CMD COMPILE env.f95
types.o: types.f95
	${RBIN} CMD COMPILE types.f95
#Registers
registers.o: registers.f95 types.o
	${RBIN} CMD COMPILE registers.f95
#Errors and warnings
errwarn.o: errwarn.f95 env.o types.o registers.o Rfuns.o
	${RBIN} CMD COMPILE errwarn.f95
#Utility functions
utils.o: utils.f95 errwarn.o
	${RBIN} CMD COMPILE utils.f95
#Math
math.o: math.f95 env.o errwarn.o Rfuns.o
	${RBIN} CMD COMPILE math.f95
#Numbers utilities
numbers_utils.o: numbers_utils.f95 env.o types.o registers.o errwarn.o utils.o
	${RBIN} CMD COMPILE numbers_utils.f95
#Nodes utilities
nodes_utils.o: nodes_utils.f95 env.o types.o errwarn.o utils.o numbers_utils.o
	${RBIN} CMD COMPILE nodes_utils.f95
#Operators
operators.o: operators.f95 env.o types.o errwarn.o math.o numbers_utils.o 
	${RBIN} CMD COMPILE operators.f95
#Node operators
node_operators.o: node_operators.f95 types.o registers.o operators.o
	${RBIN} CMD COMPILE node_operators.f95
#Nodes
nodes.o: nodes.f95 env.o types.o registers.o errwarn.o utils.o numbers_utils.o nodes_utils.o node_operators.o 
	${RBIN} CMD COMPILE nodes.f95	
#Numbers
numbers.o: numbers.f95 env.o types.o registers.o errwarn.o utils.o numbers_utils.o nodes.o
	${RBIN} CMD COMPILE numbers.f95
#Numbers modifiers
numbers_modifiers.o: numbers_modifiers.f95 env.o errwarn.o nodes.o numbers.o
	${RBIN} CMD COMPILE numbers_modifiers.f95
#Numbers math
numbers_math.o: numbers_math.f95 env.o types.o registers.o errwarn.o math.o nodes.o operators.o numbers_utils.o numbers.o 
	${RBIN} CMD COMPILE numbers_math.f95
#Optim utils
optim_utils.o: optim_utils.f95 env.o errwarn.o registers.o
	${RBIN} CMD COMPILE optim_utils.f95
#SGD
sgd.o: sgd.f95 env.o errwarn.o math.o nodes.o optim_utils.o
	${RBIN} CMD COMPILE sgd.f95
#Fortran nterfaces
interfaces.o: interfaces.f95 errwarn.o utils.o math.o nodes.o optim_utils.o
	${RBIN} CMD COMPILE interfaces.f95 
clean:
	rm *.mod
	rm *.o
