bash -x ./libguile/guile --debug -L ~/2024/03/12/guix/ -L /gnu/store/7j40c0kl651isid4pz8ylv7il9cakgw2-guile-avahi-0.4.1/share/guile/site/3.0 -L /gnu/store/vf0p9q2vryx507hl5lrsymkabd7cxrfs-guile-gcrypt-0.4.0/share/guile/site/3.0 -L /gnu/store/p5f006jcr83jc7m731vhvjdkr2j0hnp3-guile-json-4.7.3/share/guile/site/3.0 -L /gnu/store/j6c6rl14vwh7p7sz3zw5j672m4hl66f0-guile-sqlite3-0.1.3/share/guile/site/3.0 -L /gnu/store/0888y04n94lwlkpyhdbflqlsps8pqs9m-guile-gnutls-4.0.0/share/guile/site/3.0 -L /gnu/store/rvi3yajwwyrr6s2c6ggiwi8z6sy4zc9p-guile-git-0.7.0/share/guile/site/3.0 -L /gnu/store/bxvl7w7q66gbk7qkkhsiq30vl69jj4x7-guile-bytestructures-1.0.10/share/guile/site/3.0 -L /gnu/store/bsgq45p071j0ysphgywr46zzf9kdbw14-guile-ssh-0.16.4/share/guile/site/3.0 -L /gnu/store/46f352l9qnb6r2z0ah0mjwajyhlxbb1l-guile-zlib-0.1.0/share/guile/site/3.0 -L /gnu/store/pdi176v36qlp5vf24jq9zbcq14xz6vxq-guile-lzlib-0.3.0/share/guile/site/3.0 -L /gnu/store/5v0g8rhjblyg3wjynkdlbq46pz94y3cn-guile-zstd-0.1.1/share/guile/site/3.0 -L /gnu/store/jlvwpdjqawzdrl0wqsc3b94s11xbd6sm-guile-lib-0.2.8/share/guile/site/3.0 -L /gnu/store/wd5dk32fzwr64slkaqnl41w8nzp90r9b-disarchive-0.6.0/share/guile/site/3.0 -L /gnu/store/0lp207snf57l32h4w9j2k45ncjr7nrx2-guile-bzip2-0.1.0/share/guile/site/3.0 -L /gnu/store/dpf7qflvbdp8gwpi26ja800pysrbaxgi-guile-lzma-0.1.1/share/guile/site/3.0 -L ~/2024/03/12/guix/guix --no-auto-compile -e main -s ~/2024/03/12/guix/main.scm build mes --debug=999 --verbosity=9999 --no-grafts   --no-substitutes > report1.txt
#--check
sort report1.txt |uniq -c |sort -n > report2.txt
grep -P -o "([\w\-]+)" report1.txt  |sort |uniq -c | sort -n >report3.txt
