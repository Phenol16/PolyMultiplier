#!/bin/csh -f
# c shell script
# xyduan
# 20231019: init
# Please modify the PATH in rm_setup/common_setup.tcl

set cur_time = `date +"%Y%m%d%H%M"`
set dc_tag = $cur_time"_"$1

echo "set dc_tag is $dc_tag"
if (-d ./$dc_tag) then
    echo "Directory exists"
    echo "Please use a new dc_tag"
    echo "exit."
    exit 0
else
    mkdir $dc_tag
    echo "make new dir use dc_tag: $dc_tag"
endif

cd ./$dc_tag
make -f ../rm_setup/Makefile dc
touch ./$dc_tag\_dc.done

make -f ../rm_setup/Makefile fm
touch ./$dc_tag\_fm.done

