for i in `ls -d lab*`
do
    # 删除,可能在深层目录下
    find $i -name "error.txt" -exec rm -f {} \;
    find $i -name "result.txt" -exec rm -f {} \;
    find $i -name "scanner" -exec rm -f {} \;
    # *.tab.c *.tab.h *.yy.c *.yy.h
    find $i -name "*.tab.c" -exec rm -f {} \;
    find $i -name "*.tab.h" -exec rm -f {} \;
    find $i -name "*.yy.c" -exec rm -f {} \;
    find $i -name "*.yy.h" -exec rm -f {} \;
done
