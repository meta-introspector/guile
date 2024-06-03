#/time/categories/build/guix/guile/libguile/.libs/guile

gdb  --args ./libguile/.libs/guile \
    -e "(@@ (guild) main)" \
    --debug \
    -s /time/categories/build/guix/guile/meta/guild compile \
    --target=x86_64-pc-linux-gnu \
    -L /time/categories/build/guix/guile/module -o ice-9/boot-9.go ./module/ice-9/boot-9.scm

#["/time/categories/build/guix/guile/libguile/.libs/guile", "-e", "(@@ (guild) main)", "-s", "/time/categories/build/guix/guile/meta/guild", "compile", "--target=x86_64-pc-linux-gnu", "-W0", "-O1", "-L", "/time/categories/build/guix/guile/module", "-o", "ice-9/eval.go", "../module/ice-9/eval.scm"], 0x583707ce8f10 /* 79 vars */) = 0 ./libguile/guile 
