# module load gcc
FC = gfortran

OBJ1 = \
common_data_mod.o \
read_and_count_mod.o \
compute_user_field.o

#OP = -g -Wunused -fbounds-check -O3 -ffast-math -funroll-loops -ftree-vectorize -march=native -msse3 -ffree-form
OP = -Wunused -O3 -ffast-math -funroll-loops -ftree-vectorize -march=native -msse3 -ffree-form

BIN1 = compute_user_field.x

# =================================================================

clean:
	rm -f *.o *.mod *.x

# =================================================================

.f.o:
	$(FC) -c $(OP) $<

$(BIN1): $(OBJ1)
	$(FC) $(OBJ1) -o $(BIN1)
