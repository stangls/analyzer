#!/bin/bash
echo "******************************************************************************"
echo "                              REGRESSION-TESTS                                "
echo "******************************************************************************"
cd tests/regression
#for x in `find * -name "*.c" | sort`
for x in `find * -name "*loop.c" | sort`
do
  echo "Testing $x"
  ../../goblint --enable dbg.debug --enable printstats --sets result html $@ $x
  echo "------------------------------------------------------------------------------"
done
