rm output/*
mplayer -ao null $1 -vo jpeg:outdir=output
convert output/* output.gif
convert output.gif -fuzz 10% -layers Optimize optimised.gif
