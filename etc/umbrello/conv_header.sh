#!/bin/sh

src_root=messages_01
dst_root=messages_02
sed_file=cleanup_header.sed

dirs=`find $src_root -type d`
rootdir=`pwd`

for srcdir in $dirs
do
    dstdir=$rootdir/$dst_root/$srcdir
    mkdir -p $dstdir
    pushd $srcdir
    for header in *.h
    do
	dsth=$dstdir/$header
	echo "Processing $header to $dsth"
        sed -f $rootdir/$sed_file $header > $dsth
    done
    popd
done

exit 0

