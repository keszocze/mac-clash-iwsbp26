# Script to generate bit-width specific top-level Verilog modules.
#
# Part of the publication Keszocze et al., Low-level optimizations in
# high-level HDLs: Is there a benefit?, International Workshop on
# Boolean Problems, 2026.

# Iterate over register types
for m in Rotating Indexing; do
    # Iterate over counter types
    for c in OneHotCounter IndexCounter; do
	# Iterate over bit-widths
	for b in 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 20 24 28 32 36 40 44 48 52 56 60 64; do
	    # Bit-width specific top-level module name
	    designname=verilog_${b}_${m}_${c}
	    mkdir -p ./iwsbp26_verilog/$designname/
	    # Replace the string BITWIDTH with the specific bit-width
	    cat verilog_BITWIDTH_${m}_${c}.v | sed s/BITWIDTH/$b/g > ./iwsbp26_verilog/$designname/$designname.v
	done
    done
done

