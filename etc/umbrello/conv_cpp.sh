#!/bin/sh

src_root=messages_01
dst_root=messages_02
sed_file=cleanup_cpp.sed

dirs=`find $src_root -type d`
rootdir=`pwd`

for srcdir in $dirs
do
    dstdir=$rootdir/$dst_root/$srcdir
    mkdir -p $dstdir
    pushd $srcdir
    for cppfile in *.cpp
    do
	dstf=$dstdir/$cppfile
	echo "Processing $cppfile to $dstf"
        sed -f $rootdir/$sed_file $cppfile > $dstf
    done
    popd
done

exit 0

