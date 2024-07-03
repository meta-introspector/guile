#/home/mdupont/2024/06/30/guile/libguile/.libs/guile
/home/mdupont/2024/06/30/guile/libguile/.libs/guile -e "(@@ (guild) main)" \
						    -s "/home/mdupont/2024/06/30/guile/meta/guild" \
						    compile \
						    --target=x86_64-pc-linux-gnu \
						    -W0 \
						    -O1 \
						    -L /home/mdupont/2024/06/30/guile/module \
						    -o "ice-9/eval.go" \
						    ../module/ice-9/eval.scm
