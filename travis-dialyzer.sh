#!/bin/bash

PLT=plt/erlydtl-$RANDOM.plt
echo "PLT File $PLT"
export PATH=$PATH:/usr/local/bin:/usr/bin
echo "Building PLT, may take a few minutes"
dialyzer  --build_plt --apps kernel stdlib\
       --output_plt $PLT > /dev/null

echo "********************************************************************************"
for app in  erts eunit gettext syntax_tools
do 
    echo $"Adding $app"
    dialyzer --add_to_plt --apps $app\
       --plt $PLT > /dev/null
done

echo "********************************************************************************"
for app in $(ls deps/)
do
   echo "Adding $app"
   dialyzer --add_to_plt --apps deps/$app \
       --plt $PLT > /dev/null
done
echo "********************************************************************************"
echo ""

dialyzer	ebin/			\
    -Werror_handling		\
    -Wrace_conditions		\
    -Wunderspecs			\
    -Wunmatched_returns	\
    --verbose				\
    --fullpath				\
    -n						\
    --plt $PLT
#
