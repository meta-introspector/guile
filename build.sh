export PATH=/mnt/data1/nix/root/bin:$PATH
#libtasn1
#/mnt/data1/nix/root/bin/nettle-hash
RT=/mnt/data1/nix/root/
export PKG_CONFIG_LIBDIR=$RT/lib
export PKG_CONFIG_PATH=${RT}lib/pkgconfig:${RT}lib64/pkgconfig/
#/mnt/data1/nix/root/lib64/pkgconfig/nettle.pc
#/usr/local/lib/pkgconfig/libtasn1.pc
export ACLOCAL_PATH=${RT}/share/aclocal/
# sudo apt install libev-dev


#bash -x ./boostrap
#bash -x ./configure   --prefix=/mnt/data1/nix/root/
./configure   --prefix=/mnt/data1/nix/root/ 	
#make -j20
#sudo make install
